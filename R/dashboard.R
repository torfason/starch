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
    activity_description = as.character(csv_col(raw, "Activity Description")),
    activity_gear = as.character(csv_col(raw, "Activity Gear")),
    commute = as.character(csv_col(raw, "Commute")),
    # Strava writes this one in the athlete's display units (km or miles), not
    # metres; the duplicate later in the file is the metric one.
    distance_km = num(csv_col(raw, "Distance")),
    elapsed_time_s = num(csv_col(raw, "Elapsed Time")),
    moving_time_s = num(csv_col(raw, "Moving Time")),
    elevation_gain_m = num(csv_col(raw, "Elevation Gain")),
    elevation_loss_m = num(csv_col(raw, "Elevation Loss")),
    elevation_low_m = num(csv_col(raw, "Elevation Low")),
    elevation_high_m = num(csv_col(raw, "Elevation High")),
    average_grade = num(csv_col(raw, "Average Grade")),
    max_grade = num(csv_col(raw, "Max Grade")),
    average_speed_ms = num(csv_col(raw, "Average Speed")),
    max_speed_ms = num(csv_col(raw, "Max Speed")),
    average_heart_rate = num(csv_col(raw, "Average Heart Rate")),
    max_heart_rate = num(csv_col(raw, "Max Heart Rate")),
    average_cadence = num(csv_col(raw, "Average Cadence")),
    average_watts = num(csv_col(raw, "Average Watts")),
    max_watts = num(csv_col(raw, "Max Watts")),
    calories = num(csv_col(raw, "Calories")),
    relative_effort = num(csv_col(raw, "Relative Effort")),
    perceived_exertion = num(csv_col(raw, "Perceived Exertion")),
    athlete_weight_kg = num(csv_col(raw, "Athlete Weight")),
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

# Stream columns worth counting in the overview. Anything else present in a
# stream still shows up in the stream_columns listing.
stream_stat_cols <- c(
  n_rows_hr = "heartrate", n_rows_dist = "distance", n_rows_speed = "speed",
  n_rows_alt = "altitude", n_rows_gps = "lat", n_rows_cad = "cadence",
  n_rows_watts = "watts"
)

# Parquet keeps the row count and per-column-chunk null counts in the file
# footer, so the statistics below normally cost one footer read per file rather
# than decoding every column. Not all writers record null counts, hence the
# fallback in parquet_stream_stats().
stats_from_footer <- function(path) {
  md <- nanoparquet::read_parquet_metadata(path)
  cc <- md$column_chunks
  n_rows <- as.numeric(md$file_meta_data$num_rows[[1L]])
  if (is.null(cc) || nrow(cc) == 0L) {
    return(list(n_rows = n_rows, columns = character(0), nonmiss = numeric(0)))
  }
  # path_in_schema may arrive as a list column (nested schemas) or a plain
  # character vector; this handles both.
  nm <- vapply(cc$path_in_schema, function(x) paste(x, collapse = "."), character(1))
  if (n_rows > 0 && all(is.na(cc$null_count))) {
    stop("footer carries no null counts", call. = FALSE)
  }
  nulls <- ifelse(is.na(cc$null_count), 0, cc$null_count)
  nonmiss <- vapply(
    split(seq_along(nm), nm),
    function(i) sum(as.numeric(cc$num_values[i]) - as.numeric(nulls[i])),
    numeric(1)
  )
  list(n_rows = n_rows, columns = sort(unique(nm)), nonmiss = nonmiss)
}

stats_from_data <- function(path) {
  d <- nanoparquet::read_parquet(path)
  list(
    n_rows = as.numeric(nrow(d)),
    columns = names(d),
    nonmiss = vapply(d, function(x) sum(!is.na(x)), numeric(1))
  )
}

empty_stream_stats <- function() {
  cols <- stats::setNames(
    replicate(length(stream_stat_cols), numeric(0), simplify = FALSE),
    names(stream_stat_cols)
  )
  do.call(
    tibble::tibble,
    c(list(n_rows = numeric(0)), cols, list(stream_columns = character(0)))
  )
}

parquet_stream_stats <- function(paths, quiet = FALSE) {
  if (length(paths) == 0L) return(empty_stream_stats())

  use_footer <- TRUE
  rows <- vector("list", length(paths))
  if (!quiet) {
    cli::cli_progress_bar(
      "Reading stream statistics", total = length(paths), clear = FALSE
    )
  }
  for (i in seq_along(paths)) {
    s <- NULL
    if (use_footer) {
      s <- tryCatch(stats_from_footer(paths[[i]]), error = function(e) NULL)
      if (is.null(s)) {
        use_footer <- FALSE
        if (!quiet) {
          cli::cli_alert_warning(
            "Parquet footers carry no column statistics; reading columns instead (slower)"
          )
        }
      }
    }
    if (is.null(s)) s <- stats_from_data(paths[[i]])

    counts <- lapply(stream_stat_cols, function(nm) {
      if (nm %in% names(s$nonmiss)) as.numeric(s$nonmiss[[nm]]) else NA_real_
    })
    rows[[i]] <- do.call(
      tibble::tibble,
      c(
        list(n_rows = s$n_rows),
        counts,
        list(stream_columns = paste(s$columns, collapse = " "))
      )
    )
    if (!quiet) cli::cli_progress_update()
  }
  if (!quiet) cli::cli_progress_done()
  dplyr::bind_rows(rows)
}

#' Render the overview table page
#'
#' Writes `dashboard/overview_table.html`, a filterable and sortable table of
#' every activity in the manifest, joined to per-activity statistics read from
#' the Parquet layer. Each row links out to the activity on Strava, and, where
#' a detail page exists, back into the dashboard.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written page, invisibly.
#' @export
render_overview_table <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("readr", "rmarkdown", "reactable", "htmltools"))

  template <- system.file("templates", "overview_table.Rmd", package = "starch")
  if (!nzchar(template)) {
    stop("Could not locate overview_table.Rmd in the installed package.",
      call. = FALSE
    )
  }
  out <- fs::path(repo, "dashboard", "overview_table.html")
  pq_dir <- fs::path(repo, "activities_parquet")
  html_dir <- fs::path(repo, "dashboard", "activities")

  acts <- load_activities_csv(repo)
  acts$parquet <- ifelse(
    is.na(acts$stem), NA_character_,
    as.character(fs::path(pq_dir, paste0(acts$stem, ".parquet")))
  )
  acts$has_page <- !is.na(acts$stem) &
    file.exists(fs::path(html_dir, paste0(acts$stem, ".html")))

  have_pq <- !is.na(acts$parquet) & file.exists(acts$parquet)
  if (!quiet) {
    cli::cli_alert_info(
      "Reading statistics for {sum(have_pq)} of {nrow(acts)} activit{?y/ies}"
    )
  }
  t0 <- Sys.time()
  stats <- parquet_stream_stats(acts$parquet[have_pq], quiet = quiet)

  # Widen back to one row per manifest entry, leaving unconverted activities NA.
  full <- empty_stream_stats()[rep(NA_integer_, nrow(acts)), ]
  if (nrow(stats) > 0L) full[have_pq, ] <- stats
  tbl <- dplyr::bind_cols(acts, full)

  data_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(tbl, data_file)
  int_dir <- withr::local_tempdir()

  fs::dir_create(fs::path_dir(out))
  rmarkdown::render(
    input = template,
    params = list(data_path = data_file),
    output_file = as.character(fs::path_abs(out)),
    intermediates_dir = int_dir,
    knit_root_dir = int_dir,
    quiet = TRUE,
    envir = new.env()
  )
  if (!quiet) {
    cli::cli_alert_success(
      "Overview table written: {nrow(tbl)} row{?s} ({elapsed(t0)})"
    )
  }
  invisible(out)
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

  # Standalone pages, shown as cards above the activity list. Listed here so
  # that adding one is a single entry plus its renderer.
  page_specs <- list(
    list(
      file = "overview_table.html",
      title = "All activities",
      subtitle = "Filterable table"
    )
  )
  present <- Filter(
    function(p) file.exists(fs::path(repo, "dashboard", p$file)), page_specs
  )
  pages_html <- if (length(present) == 0L) {
    ""
  } else {
    card_tpl <- read_template("index_card_page.html")
    paste(vapply(present, function(p) {
      as.character(glue::glue(
        card_tpl,
        .open = "{{", .close = "}}",
        src = p$file,
        id = sub("[.]html$", "", p$file),
        title = htmltools::htmlEscape(p$title),
        subtitle = htmltools::htmlEscape(p$subtitle)
      ))
    }, character(1)), collapse = "\n")
  }

  page <- glue::glue(
    read_template("index.html"),
    .open = "{{", .close = "}}",
    count = nrow(acts),
    n_rendered = sum(has_page),
    pages_html = pages_html,
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
  render_overview_table(repo = repo, quiet = quiet)
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
