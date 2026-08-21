# Stream files the readers can dispatch on, optionally gzipped.
stream_file_regexp <- "[.](fit|gpx|tcx)([.]gz)?$"

# Content-based staleness check.
#
# Each input file gets a marker file in `hash_dir` whose *name* is the content
# hash of the input and whose *mtime* is when that content was first seen.
# Staleness is then outfile-against-marker rather than outfile-against-input,
# so a file rewritten byte-identically - which every Strava re-import does to
# the whole archive - does not trigger a rebuild. `semistale` carries the plain
# mtime comparison alongside, for comparison.
#
# A file whose conversion failed keeps an old (or absent) outfile against a
# fresh marker, so it stays stale and is retried on the next run.
content_check <- function(infiles, outfiles, hash_dir) {
  unreadable <- !fs::file_exists(infiles)
  if (any(unreadable)) {
    stop("Could not hash ", sum(unreadable), " input file(s).", call. = FALSE)
  }
  hashes <- rlang::hash_file(infiles)
  hashfiles <- fs::path(hash_dir, hashes)
  hashwrite <- !fs::file_exists(hashfiles)
  fs::dir_create(hash_dir)
  fs::file_create(hashfiles[hashwrite])

  mtime <- function(files) {
    fs::file_info(files)$modification_time |> dplyr::coalesce(.POSIXct(-Inf))
  }
  infile_mtime <- mtime(infiles)
  outfile_mtime <- mtime(outfiles)
  hashfile_mtime <- mtime(hashfiles)

  tibble::tibble(
    stale = (outfile_mtime < hashfile_mtime),
    semistale = (outfile_mtime < infile_mtime),
    hashwrite = hashwrite,
    infile_mtime, outfile_mtime, hashfile_mtime,
    infile = infiles,
    outfile = outfiles,
    hashfile = hashfiles
  )
}

#' Convert repository activities to Parquet
#'
#' Reads every stream file in a Strava repository's `activities/` directory,
#' derives the standard columns, and writes one Parquet file per activity into
#' `activities_parquet/`. Only activities whose *content* has changed are
#' rebuilt.
#'
#' @section Staleness:
#' Every import performed by [strava_zip_to_repo()] rewrites the whole archive,
#' so file modification times say nothing about whether an activity actually
#' changed. Instead, each input's content hash is recorded as an empty marker
#' file in `activities_hashes/`, named for the hash and stamped with the time
#' that content was first seen. An activity is rebuilt when its Parquet output
#' is older than that marker, which happens only when the bytes genuinely
#' differ.
#'
#' Hashing is `rlang::hash_file()`, which is XXH128 rather than md5 and is by
#' a wide margin the fastest of the file hashers available. The hash value is
#' not the point - only whether it changed - so the algorithm is free to
#' change, at the cost of one full rebuild when it does.
#'
#' Note that the hash covers the compressed file, so recompressing the archive
#' on a different machine invalidates every marker at once and forces a full
#' rebuild.
#'
#' @section Failures:
#' A stream that will not parse is a hard error that aborts the run, on the
#' basis that a file the readers cannot handle is a bug to fix rather than a
#' row to skip. The offending file is named in the error.
#'
#' Streams that parse but carry no records are written out as empty Parquet
#' files, so that they count as converted and are not retried on every run.
#'
#' @param repo Path to the Strava repository, as produced by
#'   [strava_zip_to_repo()].
#' @param max_files Maximum number of stale activities to convert in one call.
#' @param quiet Suppress progress reporting.
#'
#' @return A tibble logging one row per converted activity, invisibly.
#' @export
activity_streams_to_parquet <- function(repo = here("strava_repo"),
                                         max_files = 400,
                                         quiet = FALSE) {
  t_all <- Sys.time()

  act_dir <- fs::path(repo, "activities")
  hash_dir <- fs::path(repo, "activities_hashes")
  pq_dir <- fs::path(repo, "activities_parquet")

  if (!fs::dir_exists(act_dir)) {
    stop("No activities directory in repository:\n  ", act_dir, call. = FALSE)
  }
  if (!quiet) {
    cli::cli_h1("Converting activities to Parquet")
    cli::cli_alert_info("Repository {.path {repo}}")
  }

  ## Enumerate supported stream files, in activity-id order ------------------
  infiles <- fs::dir_ls(
    act_dir, type = "file", regexp = stream_file_regexp, ignore.case = TRUE
  )
  if (length(infiles) == 0L) {
    if (!quiet) cli::cli_alert_info("No stream files found; nothing to do")
    return(invisible(tibble::tibble()))
  }
  stems <- sub("[.].*$", "", fs::path_file(infiles))
  ids <- suppressWarnings(as.numeric(stems))
  ord <- order(ids, stems, na.last = TRUE)
  infiles <- infiles[ord]
  outfiles <- fs::path(pq_dir, paste0(stems[ord], ".parquet"))

  ## Decide what needs rebuilding -------------------------------------------
  if (!quiet) {
    cli::cli_alert_info("{length(infiles)} stream file{?s} found")
    cli::cli_alert_info("Hashing and checking staleness ...")
  }
  t0 <- Sys.time()
  checked <- content_check(infiles, outfiles, hash_dir = hash_dir)
  stale <- checked[checked$stale, ]
  if (!quiet) {
    cli::cli_alert_info(
      "{nrow(stale)} stale, {nrow(checked) - nrow(stale)} up to date ({elapsed(t0)})"
    )
  }
  if (nrow(stale) == 0L) {
    if (!quiet) cli::cli_alert_info("Nothing to convert ({elapsed(t_all)})")
    return(invisible(tibble::tibble()))
  }

  n <- min(max_files, nrow(stale))
  if (!quiet && n < nrow(stale)) {
    cli::cli_alert_info(
      "Converting the first {n} of {nrow(stale)} stale file{?s} (max_files = {max_files})"
    )
  }
  stale <- stale[seq_len(n), ]

  ## Convert -----------------------------------------------------------------
  fs::dir_create(pq_dir)
  log <- vector("list", n)
  cur_name <- ""
  t0 <- Sys.time()
  if (!quiet) {
    cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ",
        "{.file {cur_name}} | {cli::pb_eta}"
      ),
      total = n, clear = FALSE
    )
  }

  for (i in seq_len(n)) {
    infile <- stale$infile[[i]]
    outfile <- stale$outfile[[i]]
    cur_name <- fs::path_file(infile)
    if (!quiet) cli::cli_progress_update()
    notes <- character(0)

    # Zero tolerance: an unparseable stream aborts the run. The file is named
    # so it can be reproduced directly with read_stream().
    d <- tryCatch(
      read_stream(infile),
      error = function(e) {
        stop("Failed to read ", cur_name, ": ", conditionMessage(e), call. = FALSE)
      }
    )
    n_row <- nrow(d)
    n_col <- ncol(d)

    if (n_row > 0L && n_col > 0L) {
      if (!"timestamp" %in% names(d)) {
        stop("No timestamp column in ", cur_name, call. = FALSE)
      }
      if (!all(diff(d$timestamp) >= 0, na.rm = TRUE)) {
        d <- dplyr::arrange(d, .data$timestamp)
        notes <- c(notes, "timestamps out of order, sorted")
      }
      d <- addcols_time(d)
      if (all(c("lat", "lng") %in% names(d))) {
        d <- d |>
          addcols_distance() |>
          addcols_speed() |>
          addcols_latlng_offset()
      } else {
        notes <- c(notes, "no lat/lng, skipped distance and speed")
      }
      d <- relocate_activity_cols(d)
    } else {
      # Parquet needs a schema, and a 0-column tibble has none. Substitute a
      # minimal one so that every stream file still yields a readable file.
      d <- tibble::tibble(timestamp = as.POSIXct(character(0), tz = "UTC"))
      notes <- c(notes, "no records, wrote empty stream")
    }

    nanoparquet::write_parquet(d, outfile, compression = "gzip")

    if (!quiet && length(notes) > 0L) {
      cli::cli_alert_warning("{cur_name}: {paste(notes, collapse = '; ')}")
    }
    log[[i]] <- tibble::tibble(
      name = cur_name,
      nrow = n_row,
      ncol = n_col,
      names = paste(names(d), collapse = " "),
      warning_message = paste(notes, collapse = "; ")
    )
  }

  if (!quiet) {
    cli::cli_progress_done()
    cli::cli_alert_success("Converted {n} file{?s} ({elapsed(t0)})")
    cli::cli_alert_success("Done! ({elapsed(t_all)})")
  }
  invisible(dplyr::bind_rows(log))
}
