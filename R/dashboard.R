# Rendering pulls in a large publishing stack that the reader core does not
# need, so those packages live in Suggests and are checked at call time.
require_pkgs <- function(pkgs) {
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) {
    stop(
      "Install the following package(s) to render the dashboard: ",
      paste(pkgs[!ok], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Templates live in inst/templates rather than as strings in the R sources.
# That keeps them editable with the right tooling, and keeps non-ASCII glyphs
# out of the R code, which R CMD check requires to be pure ASCII.
read_template <- function(name) {
  path <- system.file("templates", name, package = "starch")
  if (!nzchar(path)) {
    stop("Could not locate template '", name, "' in the installed package.",
      call. = FALSE
    )
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Strava repeats several column names in activities.csv ("Distance", "Elapsed
# Time", ...) and readr disambiguates them with a "...N" suffix, where N is the
# column index. Matching on the stripped name and taking the first occurrence
# is position-independent; naming the suffixed columns directly is not, and
# breaks silently whenever the export's column set changes.
csv_col <- function(d, name) {
  base <- sub("[.]{3}[0-9]+$", "", names(d))
  idx <- which(base == name)
  if (length(idx) == 0L) return(rep(NA, nrow(d)))
  d[[idx[[1L]]]]
}

# Strava's activity date format varies by export vintage and locale.
parse_strava_date <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("mdy HMS p", "mdy HMS", "ymd HMS"),
    tz = "UTC",
    quiet = TRUE
  )
}

#' Read the activity manifest from a Strava export
#'
#' Reads `activities.csv` from the repository root and returns a tidy manifest,
#' newest first. The `stem` column is the activity's stream file name with all
#' extensions stripped, which is the key linking a manifest row to its Parquet
#' and HTML files. It is `NA` for manual activities that have no stream file.
#'
#' @param repo Path to the Strava repository.
#'
#' @return A tibble with one row per activity, sorted newest first.
#' @export
load_activities_csv <- function(repo = here("strava_repo")) {
  require_pkgs("readr")

  csv_path <- fs::path(repo, "activities.csv")
  if (!file.exists(csv_path)) {
    stop("No activities.csv in repository:\n  ", csv_path, call. = FALSE)
  }
  raw <- suppressMessages(
    readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)
  )

  filename <- as.character(csv_col(raw, "Filename"))
  stem <- rep(NA_character_, nrow(raw))
  has_file <- !is.na(filename) & nzchar(filename)
  stem[has_file] <- sub("[.].*$", "", fs::path_file(filename[has_file]))

  num <- function(x) suppressWarnings(as.numeric(x))

  tibble::tibble(
    activity_id = as.character(csv_col(raw, "Activity ID")),
    activity_name = as.character(csv_col(raw, "Activity Name")),
    activity_type = as.character(csv_col(raw, "Activity Type")),
    activity_date = parse_strava_date(csv_col(raw, "Activity Date")),
    distance_km = num(csv_col(raw, "Distance")),
    elapsed_time_s = num(csv_col(raw, "Elapsed Time")),
    moving_time_s = num(csv_col(raw, "Moving Time")),
    filename = filename,
    stem = stem
  ) |>
    dplyr::arrange(dplyr::desc(.data$activity_date))
}

#' Render per-activity dashboard pages
#'
#' Renders one HTML page per activity into `dashboard/activities/`, working
#' backwards from the most recent activity and stopping after `max_files`.
#'
#' An activity is rendered when it has a Parquet stream file and does not yet
#' have an HTML page. There is no content hashing at this stage: editing the
#' template or changing an activity's manifest row will not cause an existing
#' page to be rebuilt. Delete the page to force one.
#'
#' @param repo Path to the Strava repository.
#' @param max_files Maximum number of activities to render in one call.
#' @param quiet Suppress progress reporting.
#'
#' @return A tibble of the activities rendered, invisibly.
#' @export
render_activities <- function(repo = here("strava_repo"),
                              max_files = 10,
                              quiet = FALSE) {
  require_pkgs(c(
    "readr", "rmarkdown", "flexdashboard", "leaflet", "plotly", "ggplot2"
  ))

  pq_dir <- fs::path(repo, "activities_parquet")
  html_dir <- fs::path(repo, "dashboard", "activities")
  template <- system.file(
    "templates", "activity_overview.Rmd", package = "starch"
  )
  if (!nzchar(template)) {
    stop("Could not locate activity_overview.Rmd in the installed package.",
      call. = FALSE
    )
  }

  acts <- load_activities_csv(repo)
  acts <- acts[!is.na(acts$stem), ]
  acts$parquet <- fs::path(pq_dir, paste0(acts$stem, ".parquet"))
  acts$html <- fs::path(html_dir, paste0(acts$stem, ".html"))

  # Manifest rows whose stream was never converted are skipped rather than
  # failed: the Parquet step is capped too, so the two run out of step by
  # design and catch up over successive calls.
  todo <- acts[file.exists(acts$parquet) & !file.exists(acts$html), ]
  n <- min(max_files, nrow(todo))

  if (!quiet) {
    cli::cli_alert_info(
      "{nrow(todo)} activit{?y/ies} awaiting a page; rendering {n}"
    )
  }
  if (n == 0L) {
    return(invisible(todo[0, ]))
  }
  todo <- todo[seq_len(n), ]

  fs::dir_create(html_dir)
  int_dir <- withr::local_tempdir()
  t0 <- Sys.time()
  cur_name <- ""
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
    cur_name <- paste0(todo$stem[[i]], ".html")
    if (!quiet) cli::cli_progress_update()
    rmarkdown::render(
      input = template,
      params = list(
        activity_id = todo$activity_id[[i]],
        activity_name = todo$activity_name[[i]],
        activity_type = todo$activity_type[[i]],
        parquet_path = as.character(fs::path_abs(todo$parquet[[i]])),
        smooth_window = 5
      ),
      output_file = as.character(fs::path_abs(todo$html[[i]])),
      # The template lives in the installed package, which is read-only, so
      # knitr's intermediates must be sent somewhere writable.
      intermediates_dir = int_dir,
      knit_root_dir = int_dir,
      quiet = TRUE,
      envir = new.env()
    )
  }

  if (!quiet) {
    cli::cli_progress_done()
    cli::cli_alert_success("Rendered {n} page{?s} ({elapsed(t0)})")
  }
  invisible(todo)
}

#' Render the dashboard index
#'
#' Writes `dashboard/index.html`, a sidebar of every activity in the manifest
#' beside a viewer pane. Activities without a rendered page are shown dimmed
#' and are not selectable, so the index is usable long before every page
#' exists.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written index, invisibly.
#' @export
render_index <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("readr", "glue", "htmltools"))

  html_dir <- fs::path(repo, "dashboard", "activities")
  out <- fs::path(repo, "dashboard", "index.html")

  acts <- load_activities_csv(repo)
  page_rel <- ifelse(
    is.na(acts$stem), NA_character_, paste0("activities/", acts$stem, ".html")
  )
  has_page <- !is.na(acts$stem) &
    file.exists(fs::path(html_dir, paste0(acts$stem, ".html")))

  cards <- glue::glue_data(
    list(
      id = htmltools::htmlEscape(acts$activity_id),
      src = ifelse(has_page, page_rel, ""),
      cls = ifelse(has_page, "card", "card nopage"),
      date_str = format(acts$activity_date, "%Y-%m-%d"),
      time_str = format(acts$activity_date, "%H:%M"),
      name = htmltools::htmlEscape(acts$activity_name),
      type = htmltools::htmlEscape(acts$activity_type),
      stats = fmt_card_stats(
        acts$distance_km, acts$elapsed_time_s, acts$activity_type
      )
    ),
    read_template("index_card.html"),
    .open = "{{", .close = "}}"
  ) |> paste(collapse = "\n")

  page <- glue::glue(
    read_template("index.html"),
    .open = "{{", .close = "}}",
    count = nrow(acts),
    n_pages = sum(has_page),
    cards_html = cards
  )

  fs::dir_create(fs::path_dir(out))
  writeLines(page, out)
  if (!quiet) {
    cli::cli_alert_success(
      "Index written: {nrow(acts)} activit{?y/ies}, {sum(has_page)} with pages"
    )
  }
  invisible(out)
}

#' Render the activity dashboard
#'
#' Renders outstanding activity pages, then rebuilds the index so that it
#' reflects them. The index is always rewritten; it is a single cheap file.
#'
#' @inheritParams render_activities
#'
#' @return Path to the index, invisibly.
#' @export
render_dashboard <- function(repo = here("strava_repo"),
                             max_files = 10,
                             quiet = FALSE) {
  t_all <- Sys.time()
  if (!quiet) {
    cli::cli_h1("Rendering dashboard")
    cli::cli_alert_info("Repository {.path {repo}}")
  }
  render_activities(repo = repo, max_files = max_files, quiet = quiet)
  out <- render_index(repo = repo, quiet = quiet)
  if (!quiet) cli::cli_alert_success("Done ({elapsed(t_all)})")
  invisible(out)
}

#' Open the dashboard in a browser
#'
#' @param repo Path to the Strava repository.
#'
#' @return Path to the index, invisibly.
#' @export
view_dashboard <- function(repo = here("strava_repo")) {
  index <- fs::path(repo, "dashboard", "index.html")
  if (!file.exists(index)) {
    stop(
      "No dashboard index at:\n  ", index, "\nRun render_dashboard() first.",
      call. = FALSE
    )
  }
  utils::browseURL(as.character(fs::path_abs(index)))
  invisible(index)
}

# ---- index formatting ------------------------------------------------------

fmt_pace <- function(min_per_km) {
  if (!is.finite(min_per_km)) return("\u2014")
  mins <- floor(min_per_km)
  secs <- round((min_per_km - mins) * 60)
  if (secs == 60L) {
    mins <- mins + 1L
    secs <- 0L
  }
  sprintf("%d:%02d", mins, secs)
}

fmt_duration <- function(s) {
  if (!is.finite(s)) return("\u2014")
  h <- floor(s / 3600)
  m <- floor((s %% 3600) / 60)
  ss <- round(s %% 60)
  if (h > 0) sprintf("%dh%02dm", h, m) else sprintf("%dm%02ds", m, ss)
}

# Pace is only shown for foot-based activities, where it is the natural unit.
fmt_card_stats <- function(distance_km, elapsed_time_s, activity_type) {
  vapply(seq_along(distance_km), function(i) {
    dist <- distance_km[[i]]
    secs <- elapsed_time_s[[i]]
    dur <- fmt_duration(secs)
    if (!is.finite(dist) || dist == 0) return(dur)
    out <- paste0(sprintf("%.1f km", dist), " \u00b7 ", dur)
    foot <- !is.na(activity_type[[i]]) &&
      activity_type[[i]] %in% c("Run", "Trail Run", "Walk", "Hike")
    if (foot && is.finite(secs)) {
      out <- paste0(out, " \u00b7 ", fmt_pace((secs / 60) / dist), "/km")
    }
    out
  }, character(1))
}
