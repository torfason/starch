# Small helpers that belong to no particular layer.

# Hash each row of a data frame, returning one hash per row.
#
# vec_chop() splits the frame into a list of one-row frames in one pass, rather
# than subsetting per row, and this formulation benchmarks an order of
# magnitude faster than the obvious alternatives (pasting each row into a
# string, or applying a hasher column-wise and combining). rlang::hash() is
# likewise the fastest of the available object hashers, so the pair is what the
# package uses wherever row-level change detection is wanted.
#
# The hash covers types and attributes as well as values, so two rows that
# print alike but differ in storage type hash differently. That is the wanted
# behaviour here: a column whose type changed is a change.
hash_rows <- function(d, hash_func = rlang::hash) {
  stopifnot(is.data.frame(d))
  stopifnot(is.function(hash_func))

  d |>
    vctrs::vec_chop() |>
    vapply(hash_func, character(1))
}


# --- Hash sidecars ------------------------------------------------------------
#
# Every generated artefact records the hashes of the inputs that produced it, in
# a small DCF file alongside `<repo>/hashes/<stage>/<key>.dcf`. One field per
# ancestor, named for what it is:
#
#     stream: 8f2c1a0b4d6e9037a5c8b1f2e4d70a96
#
# An artefact is stale when it is absent, when its sidecar is absent, or when a
# recorded hash differs from the input's hash now. No modification times are
# involved, and the sidecar is written only once the artefact is safely on disk,
# so an interrupted or failed step leaves no record and is retried.
#
# One file per artefact rather than one manifest per stage: a manifest would be
# one read and one write instead of hundreds of tiny files, but it would also
# need coordinating between workers, whereas a sidecar is written by whoever
# wrote the artefact and by no one else.
#
# DCF is base R and reads as plain text. jsonlite reads a two-field file at the
# same speed but allocates a hundredfold more, and is only a Suggests.

hash_stage_dir <- function(repo, stage) {
  fs::path(repo, "hashes", stage)
}

hash_path <- function(repo, stage, key) {
  fs::path(hash_stage_dir(repo, stage), paste0(key, ".dcf"))
}

# Reads one sidecar into a named character vector, or an empty one when the file
# is absent or unreadable.
#
# A malformed sidecar never errors: read.dcf() is lenient enough to parse most
# junk as some field or other, and what it cannot parse comes back here as
# empty. Either way the fields the caller asked for are missing or wrong, so the
# artefact reads as stale - the safe direction.
hash_read_one <- function(path) {
  if (!file.exists(path)) return(character(0))
  d <- tryCatch(read.dcf(path), error = function(e) NULL)
  if (is.null(d) || nrow(d) != 1L) return(character(0))
  out <- as.character(d[1, ])
  names(out) <- colnames(d)
  out
}

hash_write_one <- function(path, hashes) {
  fs::dir_create(fs::path_dir(path))
  write.dcf(as.data.frame(as.list(hashes), stringsAsFactors = FALSE), path)
  invisible(path)
}

# Compare recorded ancestor hashes against current ones.
#
# `current` is a data frame with one row per key and one column per ancestor,
# holding the hashes as they are now. `reason` says why a row is stale, so the
# caller can report which input moved rather than only that something did.
hash_check <- function(keys, outfiles, hashfiles, current) {
  stopifnot(is.data.frame(current))
  n <- length(keys)
  stopifnot(length(outfiles) == n, length(hashfiles) == n, nrow(current) == n)

  fields <- names(current)
  reason <- character(n)

  have_out <- fs::file_exists(outfiles)
  for (i in seq_len(n)) {
    if (!have_out[[i]]) {
      reason[[i]] <- "missing"
      next
    }
    was <- hash_read_one(hashfiles[[i]])
    if (length(was) == 0L) {
      reason[[i]] <- "unrecorded"
      next
    }
    now <- as.character(unlist(current[i, ], use.names = FALSE))
    differs <- fields[is.na(was[fields]) | was[fields] != now]
    reason[[i]] <- paste(differs, collapse = ",")
  }

  tibble::tibble(
    key = as.character(keys),
    stale = nzchar(reason),
    reason = reason,
    outfile = outfiles,
    hashfile = hashfiles,
    current
  )
}

# The shape hash_check() returns, with no rows. Callers need this for the empty
# case, because paste0() treats a zero-length vector as "" and would otherwise
# manufacture a single entry out of nothing.
empty_staleness <- function(fields) {
  out <- tibble::tibble(
    key = character(0),
    stale = logical(0),
    reason = character(0),
    outfile = fs::path(character(0)),
    hashfile = fs::path(character(0))
  )
  for (f in fields) out[[f]] <- character(0)
  out
}

# One line summarising a staleness check, e.g.
#   "50 stale (12 missing, 38 stream)"
hash_reason_summary <- function(checked) {
  r <- checked$reason[checked$stale]
  if (length(r) == 0L) return("")
  tab <- sort(table(r), decreasing = TRUE)
  paste(paste0(unname(tab), " ", names(tab)), collapse = ", ")
}


# --- Superseded: the content-marker staleness check ---------------------------
#
# Replaced by the hash sidecars above, and kept, unused, for reference.
#
# Each input file got a marker file in `hash_dir` whose *name* was the content
# hash of the input and whose *mtime* was when that content was first seen.
# Staleness was then outfile-against-marker rather than outfile-against-input,
# so a file rewritten byte-identically - which every Strava re-import does to
# the whole archive - did not trigger a rebuild. `semistale` carried the plain
# mtime comparison alongside, for comparison.
#
# Two things were wrong with it. The markers were keyed by content globally
# rather than per activity, so an activity that changed to content that another
# activity had already produced found a marker with an old mtime and was not
# rebuilt. And comparing modification times is a proxy for comparing hashes,
# which the sidecars now do directly. A third, smaller problem: the directory
# accumulated one marker per content ever seen and grew without bound.
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
