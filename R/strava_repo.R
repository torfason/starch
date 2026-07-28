# Human-readable elapsed time since `t0`, for progress reporting.
elapsed <- function(t0) {
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (secs < 60) sprintf("%.1fs", secs) else sprintf("%.1f min", secs / 60)
}

# Generated artefacts that live inside the repo but are not part of the
# imported export, and so are never committed. Written to .gitignore when a
# repository is initialised; edit here if the set of artefacts changes.
strava_repo_ignore <- c(
  "activities_parquet/",
  "activities_hash/"
)

#' Locate the most recent Strava export archive
#'
#' Strava bulk exports arrive as a zip archive. This picks the most recent one
#' out of a directory of downloaded exports, and is the default source for
#' [strava_zip_to_repo()].
#'
#' "Most recent" can mean either the last file name or the last modification
#' time. These usually agree, but need not: copying or restoring files rewrites
#' modification times, and name ordering only tracks chronology if the archives
#' are named so that they sort that way. The default (`"both"`) requires the two
#' to agree and errors when they do not, rather than silently picking one.
#'
#' @param dir Directory holding downloaded Strava export archives.
#' @param by How the most recent archive is identified. `"both"` (the default)
#'   requires the name and modification time orderings to agree, `"name"` uses
#'   the file name alone, and `"time"` the modification time alone.
#'
#' @return Path to the most recent `.zip` archive in `dir`.
#' @importFrom here here
#' @export
latest_strava_zip <- function(dir = here("strava_zips"),
                              by = c("both", "name", "time")) {
  by <- match.arg(by)

  if (!fs::dir_exists(dir)) {
    stop("Strava zip directory not found: ", dir, call. = FALSE)
  }
  zips <- fs::dir_ls(dir, type = "file", glob = "*.zip")
  if (length(zips) == 0L) {
    stop("No .zip archives found in: ", dir, call. = FALSE)
  }

  # Radix ordering keeps the name comparison locale-independent.
  ord_name <- zips[order(basename(zips), method = "radix")]
  ord_time <- zips[order(fs::file_info(zips)$modification_time)]
  last_name <- ord_name[[length(ord_name)]]
  last_time <- ord_time[[length(ord_time)]]

  out <- switch(by,
    name = last_name,
    time = last_time,
    both = {
      if (last_name != last_time) {
        stop(
          "Most recent Strava zip is ambiguous:\n",
          "  by name: ", basename(last_name), "\n",
          "  by time: ", basename(last_time), "\n",
          'Pass by = "name" or by = "time" to choose explicitly.',
          call. = FALSE
        )
      }
      last_name
    }
  )
  unname(out)
}

#' Import a Strava export archive into a version-controlled repository
#'
#' Extracts a Strava bulk export into a git repository, gzips the track files
#' that arrived uncompressed, and commits the result. Running it against
#' successive exports builds a version-controlled history of the archive, in
#' which each commit is one export and the diff is whatever actually changed.
#'
#' Every export is extracted in full, overwriting what is already there.
#' Activities can be edited in Strava after the fact, so the presence of a file
#' in the repository says nothing about whether its content is current. Git,
#' not the file system, determines what changed.
#'
#' @section Repository preconditions:
#' Because the import overwrites files without prompting, `repo` must be in one
#' of exactly two states, checked before anything is written:
#'
#' * **Empty or absent** - the directory is created, a repository is
#'   initialised, and a `.gitignore` is written.
#' * **A git repository with a clean working tree** - no staged changes, no
#'   unstaged changes, and no untracked files. Ignored files do not count.
#'
#' Anything else is an error, on the assumption that it is an unrelated
#' directory. The clean-tree requirement also makes a failed import
#' recoverable: nothing predates the run, so `git reset --hard` undoes it.
#'
#' Archives containing entries under `.git/`, absolute paths, or parent
#' traversal are rejected outright.
#'
#' @param zip Path to a Strava export archive. Defaults to the most recent
#'   archive found by [latest_strava_zip()].
#' @param repo Path to the repository the export is imported into.
#' @param commit Whether to commit the result. When `FALSE` the changes are
#'   left in the working tree, which will block the next import until they are
#'   dealt with.
#' @param author Optional git signature for the commit, as accepted by
#'   [gert::git_commit()]. When `NULL`, the configured git identity is used and
#'   its absence is reported before any files are written.
#' @param quiet Suppress progress reporting.
#'
#' @return The `gert::git_status()` table of what changed, invisibly.
#' @export
strava_zip_to_repo <- function(zip = latest_strava_zip(),
                               repo = here("strava_repo"),
                               commit = TRUE,
                               author = NULL,
                               quiet = FALSE) {
  t_all <- Sys.time()
  if (!file.exists(zip)) {
    stop("Strava zip not found: ", zip, call. = FALSE)
  }

  ## Inspect the archive before touching the file system --------------------
  if (!quiet) {
    cli::cli_h1("Importing Strava export")
    cli::cli_alert_info("Archive {.file {basename(zip)}}")
  }
  entries <- zip::zip_list(zip)$filename
  paths <- gsub("\\\\", "/", entries)
  n_entries <- length(paths)
  if (!quiet) {
    n_tracks <- sum(grepl("[.](fit|gpx|tcx)([.]gz)?$", paths, ignore.case = TRUE))
    cli::cli_alert_info("{n_entries} entr{?y/ies}, of which {n_tracks} track file{?s}")
  }

  unsafe <-
    grepl("(^|/)[.]git(/|$)", paths) |  # repository metadata
    grepl("^(/|[A-Za-z]:)", paths) |    # absolute paths
    grepl("(^|/)[.][.](/|$)", paths)    # parent traversal
  if (any(unsafe)) {
    shown <- paths[unsafe][seq_len(min(5L, sum(unsafe)))]
    stop(
      "Refusing to extract: archive contains unsafe entries.\n",
      paste0("  ", shown, collapse = "\n"),
      if (sum(unsafe) > 5L) paste0("\n  ... and ", sum(unsafe) - 5L, " more"),
      call. = FALSE
    )
  }

  ## Repository preconditions ----------------------------------------------
  repo_exists <- fs::dir_exists(repo)
  contents <- if (repo_exists) fs::dir_ls(repo, all = TRUE) else character(0)

  if (!repo_exists || length(contents) == 0L) {
    fs::dir_create(repo)
    gert::git_init(repo)
    writeLines(strava_repo_ignore, fs::path(repo, ".gitignore"))
    if (!quiet) cli::cli_alert_success("Initialised repository at {.path {repo}}")
  } else {
    if (!file.exists(fs::path(repo, ".git"))) {
      stop(
        "Repository directory is not empty and is not a git repository:\n  ",
        repo, "\n",
        "Refusing to overwrite files in what may be an unrelated directory.",
        call. = FALSE
      )
    }
    if (!quiet) cli::cli_alert_info("Checking working tree at {.path {repo}} ...")
    t0 <- Sys.time()
    if (nrow(gert::git_status(repo = repo)) > 0L) {
      stop(
        "Repository has uncommitted changes:\n  ", repo, "\n",
        "Commit, stash, or discard them before importing.",
        call. = FALSE
      )
    }
    if (!quiet) cli::cli_alert_success("Working tree clean ({elapsed(t0)})")
  }

  # Fail now rather than after a full extraction if the commit could not be made.
  if (commit && is.null(author)) {
    cfg <- gert::git_config(repo = repo)
    missing_id <- setdiff(c("user.name", "user.email"), cfg$name)
    if (length(missing_id) > 0L) {
      stop(
        "No git identity configured (missing ",
        paste(missing_id, collapse = " and "), ").\n",
        "Set one with gert::git_config_global_set(), pass author =, ",
        "or call with commit = FALSE.",
        call. = FALSE
      )
    }
  }

  ## Extract, overwriting whatever is already there -------------------------
  if (!quiet) cli::cli_alert_info("Extracting {n_entries} entr{?y/ies} ...")
  t0 <- Sys.time()
  zip::unzip(zip, exdir = repo)
  if (!quiet) cli::cli_alert_success("Extracted ({elapsed(t0)})")

  ## Compress whatever arrived uncompressed ---------------------------------
  # Entries that are already .gz - most tracks - are left exactly as Strava
  # produced them. Only the plain files are gzipped, by the same method used
  # for the fixtures in data-raw/testfiles.R. Working from the archive listing
  # rather than the extracted tree confines this to what this run wrote.
  extracted <- fs::path(repo, paths)
  plain <- extracted[grepl("[.](fit|gpx|tcx)$", paths, ignore.case = TRUE)]
  plain <- plain[file.exists(plain)]
  if (length(plain) == 0L) {
    if (!quiet) cli::cli_alert_info("No uncompressed track files to compress")
  } else {
    t0 <- Sys.time()
    if (!quiet) {
      cli::cli_progress_bar(
        "Compressing tracks", total = length(plain), clear = FALSE
      )
    }
    for (f in plain) {
      R.utils::gzip(f, overwrite = TRUE, remove = TRUE, compression = 9)
      if (!quiet) cli::cli_progress_update()
    }
    if (!quiet) {
      cli::cli_progress_done()
      cli::cli_alert_success(
        "Compressed {length(plain)} file{?s} ({elapsed(t0)})"
      )
    }
  }

  ## Commit ------------------------------------------------------------------
  # Both this walk and the staging below are O(files in the repo) inside
  # libgit2, and on a full export each can take minutes.
  if (!quiet) cli::cli_alert_info("Scanning repository for changes ...")
  t0 <- Sys.time()
  status <- gert::git_status(repo = repo)
  if (!quiet) {
    cli::cli_alert_success("{nrow(status)} change{?s} found ({elapsed(t0)})")
  }
  if (nrow(status) == 0L) {
    if (!quiet) cli::cli_alert_info("Nothing to commit; import complete")
    return(invisible(status))
  }
  if (commit) {
    # Staging everything is safe: the working tree was verified clean above, so
    # nothing staged here predates this import, and .gitignore holds back the
    # generated artefacts.
    if (!quiet) cli::cli_alert_info("Staging and committing ...")
    t0 <- Sys.time()
    gert::git_add(".", repo = repo)
    sha <- gert::git_commit(
      message = sprintf(
        "Import %s (%d file%s changed)",
        basename(zip), nrow(status), if (nrow(status) == 1L) "" else "s"
      ),
      repo = repo,
      author = author
    )
    if (!quiet) {
      cli::cli_alert_success("Committed {.val {substr(sha, 1, 10)}} ({elapsed(t0)})")
    }
  } else if (!quiet) {
    cli::cli_alert_warning(
      "Left uncommitted; the next import will refuse to run until this is resolved"
    )
  }
  if (!quiet) cli::cli_alert_success("Import complete ({elapsed(t_all)})")
  invisible(status)
}
