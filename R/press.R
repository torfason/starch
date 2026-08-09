# The one-call maintenance run: import, convert, render, view. It sits above
# all three dashboard stacks rather than inside any of them, so nothing here is
# prefixed, and it calls the qs_ renderers by name.

# Prompt helpers. readline() returns "" without blocking under Rscript, so
# every path that reaches one of these is gated on interactive() first.
press_ask <- function(prompt, choices, default) {
  labs <- ifelse(choices == default, paste0("[", choices, "]"), choices)
  repeat {
    ans <- tolower(trimws(readline(
      paste0(prompt, " ", paste(labs, collapse = "/"), ": ")
    )))
    if (!nzchar(ans)) return(default)
    # "no" is the natural answer to a yes/skip question, and means skip.
    if (ans %in% c("n", "no") && "skip" %in% choices) return("skip")
    hit <- choices[startsWith(choices, ans)]
    if (length(hit) == 1L) return(hit)
    cli::cli_alert_warning("Answer one of: {.val {choices}}")
  }
}

# Returns NA to mean abort, so the caller distinguishes it from 0, which is a
# legitimate "run this step but do no work".
press_ask_count <- function(prompt, default) {
  repeat {
    ans <- trimws(readline(sprintf("%s [%d]/abort: ", prompt, default)))
    if (!nzchar(ans)) return(default)
    if (tolower(ans) %in% c("a", "abort")) return(NA_integer_)
    n <- suppressWarnings(as.integer(ans))
    if (!is.na(n) && n >= 0) return(n)
    cli::cli_alert_warning("Enter a number, or {.val abort}")
  }
}

press_aborted <- function() {
  cli::cli_alert_warning("Aborted; nothing further was run")
  invisible(NULL)
}


#' Run the maintenance sequence
#'
#' Imports the latest Strava export, converts new activities to Parquet,
#' renders the static dashboard, and offers to open it. Each step reports what
#' it is about to do and asks before doing it, so the whole run can be stepped
#' through, skipped past, or abandoned.
#'
#' @param repo Path to the Strava repository.
#' @param zip Path to the export archive. Defaults to the most recent one, and
#'   the import step is skipped if none is found.
#' @param max_parquet Default number of activities to convert to Parquet.
#' @param max_pages Default number of dashboard pages to render.
#' @param max_points Stream points kept per page, passed to the renderer.
#' @param confirm Ask before each step. `FALSE` runs the whole sequence on the
#'   defaults without prompting.
#' @param view Offer to open the dashboard at the end.
#' @param quiet Suppress the underlying functions' progress reporting. The
#'   prompts and step summaries are not affected.
#'
#' @return Path to the repository, invisibly, or `NULL` if aborted.
#' @export
press <- function(repo = here("strava_repo"),
                  zip = latest_strava_zip(),
                  max_parquet = 50,
                  max_pages = 10,
                  max_points = 600,
                  confirm = TRUE,
                  view = TRUE,
                  quiet = FALSE) {
  # Checked before any work: both prompting and browsing are meaningless in a
  # non-interactive session, and failing at the end of a long run would be a
  # poor way to find that out.
  if (!interactive() && (confirm || view)) {
    stop(
      "press() needs an interactive session unless confirm and view are both ",
      "FALSE.",
      call. = FALSE
    )
  }

  cli::cli_h1("starch maintenance")
  cli::cli_alert_info("Repository {.path {repo}}")

  ## Import ------------------------------------------------------------------
  cli::cli_h2("Import export archive")
  # zip is a promise, so a missing directory surfaces here rather than at the
  # call site, and is a reason to skip the step rather than to fail.
  zip_path <- tryCatch(zip, error = function(e) NULL)
  if (is.null(zip_path) || !file.exists(zip_path)) {
    cli::cli_alert_warning("No export archive found; skipping import")
  } else {
    cli::cli_alert_info("Archive {.file {fs::path_file(zip_path)}}")
    cli::cli_alert_info(
      "{round(file.size(zip_path) / 1024^2, 1)} MB, dated \\
       {format(file.mtime(zip_path), '%Y-%m-%d %H:%M')}"
    )
    ans <- if (confirm) {
      press_ask("Import into the repository?", c("yes", "skip", "abort"), "yes")
    } else {
      "yes"
    }
    if (ans == "abort") return(press_aborted())
    if (ans == "yes") {
      strava_zip_to_repo(zip = zip_path, repo = repo, quiet = quiet)
    }
  }

  ## Convert -----------------------------------------------------------------
  cli::cli_h2("Convert activities to Parquet")
  # list.files() rather than fs::dir_ls(): a repository that has not been
  # imported into yet has neither directory, and this is a summary line, not a
  # reason to stop before the prompt.
  n_streams <- length(list.files(
    fs::path(repo, "activities"),
    pattern = stream_file_regexp, ignore.case = TRUE
  ))
  n_parquet <- length(list.files(
    fs::path(repo, "activities_parquet"),
    pattern = "[.]parquet$", ignore.case = TRUE
  ))
  cli::cli_alert_info(
    "{n_streams} stream file{?s}, {n_parquet} already converted"
  )
  n <- if (confirm) {
    press_ask_count("How many to convert?", max_parquet)
  } else {
    max_parquet
  }
  if (is.na(n)) return(press_aborted())
  if (n > 0L) {
    strava_activities_to_parquet(repo, max_files = n, quiet = quiet)
  }

  ## Render ------------------------------------------------------------------
  cli::cli_h2("Render the static dashboard")
  acts <- load_activities_csv(repo)
  acts <- acts[!is.na(acts$stem), ]
  has_pq <- file.exists(
    fs::path(repo, "activities_parquet", paste0(acts$stem, ".parquet"))
  )
  has_page <- file.exists(
    fs::path(repo, "dashboard_qs", paste0(acts$stem, ".html"))
  )
  cli::cli_alert_info(
    "{sum(has_pq & !has_page)} activit{?y/ies} awaiting a page, \\
     {sum(has_page)} already rendered"
  )
  cli::cli_alert_info("Overview pages and the index are rebuilt either way")
  n <- if (confirm) {
    press_ask_count("How many pages to render?", max_pages)
  } else {
    max_pages
  }
  if (is.na(n)) return(press_aborted())
  qs_render_dashboard(
    repo,
    max_files = n, max_points = max_points, quiet = quiet
  )

  ## View --------------------------------------------------------------------
  if (view) {
    cli::cli_h2("View")
    ans <- press_ask("Open the dashboard now?", c("yes", "no"), "yes")
    if (ans == "yes") qs_view_dashboard(repo)
  }

  cli::cli_alert_success("Done")
  invisible(repo)
}
