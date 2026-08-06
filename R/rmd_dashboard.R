# Templates live in inst/rmd_templates rather than as strings in the R sources.
# That keeps them editable with the right tooling, and keeps non-ASCII glyphs
# out of the R code, which R CMD check requires to be pure ASCII.
rmd_read_template <- function(name) {
  path <- system.file("rmd_templates", name, package = "starch")
  if (!nzchar(path)) {
    stop("Could not locate template '", name, "' in the installed package.",
      call. = FALSE
    )
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}


#' Render per-activity dashboard pages
#'
#' Renders one HTML page per activity into `dashboard_rmd/activities/`, working
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
rmd_render_activities <- function(repo = here("strava_repo"),
                              max_files = 10,
                              quiet = FALSE) {
  require_pkgs(c(
    "readr", "rmarkdown", "flexdashboard", "leaflet", "plotly", "ggplot2"
  ))

  pq_dir <- fs::path(repo, "activities_parquet")
  html_dir <- fs::path(repo, "dashboard_rmd", "activities")
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


#' Render the overview table page
#'
#' Writes `dashboard_rmd/overview_table.html`, a filterable and sortable table of
#' every activity in the manifest, joined to per-activity statistics read from
#' the Parquet layer. Each row links out to the activity on Strava, and, where
#' a detail page exists, back into the dashboard.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written page, invisibly.
#' @export
rmd_render_overview_table <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("readr", "rmarkdown", "reactable", "htmltools"))

  template <- system.file("rmd_templates", "overview_table.Rmd", package = "starch")
  if (!nzchar(template)) {
    stop("Could not locate overview_table.Rmd in the installed package.",
      call. = FALSE
    )
  }
  out <- fs::path(repo, "dashboard_rmd", "overview_table.html")
  pq_dir <- fs::path(repo, "activities_parquet")
  html_dir <- fs::path(repo, "dashboard_rmd", "activities")

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

#' Render the trends page
#'
#' Writes `dashboard_rmd/trends.html`, an interactive view of totals over time.
#' Distance, activity count, elapsed time or moving time are bucketed by week
#' or month over a period measured back from the newest activity, for any
#' selection of sports. It is built entirely from `activities.csv`, so it is
#' complete across the whole history even while most activities lack a detail
#' page. The controls re-aggregate client-side; there is no server component.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written page, invisibly.
#' @export
rmd_render_trends <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("readr", "rmarkdown", "plotly", "jsonlite"))

  template <- system.file("rmd_templates", "trends.Rmd", package = "starch")
  if (!nzchar(template)) {
    stop("Could not locate trends.Rmd in the installed package.",
      call. = FALSE
    )
  }
  out <- fs::path(repo, "dashboard_rmd", "trends.html")

  t0 <- Sys.time()
  acts <- load_activities_csv(repo)

  # The page needs only date, sport and the four metric channels; all
  # aggregation happens in the browser. Dates are formatted in UTC to match the
  # parsing in load_activities_csv() and keep client-side bucketing deterministic.
  trends <- tibble::tibble(
    date = format(acts$activity_date, "%Y-%m-%d", tz = "UTC"),
    type = acts$activity_type,
    distance_km = acts$distance_km,
    elapsed_s = acts$elapsed_time_s,
    moving_s = acts$moving_time_s
  )
  # An activity with no date or no type cannot be placed or grouped.
  keep <- !is.na(trends$date) & !is.na(trends$type)
  trends <- trends[keep, , drop = FALSE]

  data_file <- withr::local_tempfile(fileext = ".rds")
  saveRDS(trends, data_file)
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
      "Trends page written: {nrow(trends)} activit{?y/ies} ({elapsed(t0)})"
    )
  }
  invisible(out)
}

#' Render the dashboard index
#'
#' Writes `dashboard_rmd/index.html`, a sidebar of every activity in the manifest
#' beside a viewer pane. Activities without a rendered page are shown dimmed
#' and are not selectable, so the index is usable long before every page
#' exists.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written index, invisibly.
#' @export
rmd_render_index <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("readr", "glue", "htmltools"))

  html_dir <- fs::path(repo, "dashboard_rmd", "activities")
  out <- fs::path(repo, "dashboard_rmd", "index.html")

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
      stats = rmd_fmt_card_stats(
        acts$distance_km, acts$elapsed_time_s, acts$activity_type
      )
    ),
    rmd_read_template("index_card.html"),
    .open = "{{", .close = "}}"
  ) |> paste(collapse = "\n")

  # Standalone pages, shown as cards above the activity list. Listed here so
  # that adding one is a single entry plus its renderer.
  page_specs <- list(
    list(
      file = "trends.html",
      title = "Trends",
      subtitle = "Totals over time"
    ),
    list(
      file = "overview_table.html",
      title = "All activities",
      subtitle = "Filterable table"
    )
  )
  present <- Filter(
    function(p) file.exists(fs::path(repo, "dashboard_rmd", p$file)), page_specs
  )
  pages_html <- if (length(present) == 0L) {
    ""
  } else {
    card_tpl <- rmd_read_template("index_card_page.html")
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
    rmd_read_template("index.html"),
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
#' @inheritParams rmd_render_activities
#'
#' @return Path to the index, invisibly.
#' @export
rmd_render_dashboard <- function(repo = here("strava_repo"),
                             max_files = 10,
                             quiet = FALSE) {
  t_all <- Sys.time()
  if (!quiet) {
    cli::cli_h1("Rendering dashboard")
    cli::cli_alert_info("Repository {.path {repo}}")
  }
  rmd_render_activities(repo = repo, max_files = max_files, quiet = quiet)
  rmd_render_overview_table(repo = repo, quiet = quiet)
  rmd_render_trends(repo = repo, quiet = quiet)
  out <- rmd_render_index(repo = repo, quiet = quiet)
  if (!quiet) cli::cli_alert_success("Done ({elapsed(t_all)})")
  invisible(out)
}

#' Open the dashboard in a browser
#'
#' @param repo Path to the Strava repository.
#'
#' @return Path to the index, invisibly.
#' @export
rmd_view_dashboard <- function(repo = here("strava_repo")) {
  index <- fs::path(repo, "dashboard_rmd", "index.html")
  if (!file.exists(index)) {
    stop(
      "No dashboard index at:\n  ", index, "\nRun rmd_render_dashboard() first.",
      call. = FALSE
    )
  }
  utils::browseURL(as.character(fs::path_abs(index)))
  invisible(index)
}

# ---- index formatting ------------------------------------------------------

rmd_fmt_pace <- function(min_per_km) {
  if (!is.finite(min_per_km)) return("\u2014")
  mins <- floor(min_per_km)
  secs <- round((min_per_km - mins) * 60)
  if (secs == 60L) {
    mins <- mins + 1L
    secs <- 0L
  }
  sprintf("%d:%02d", mins, secs)
}

rmd_fmt_duration <- function(s) {
  if (!is.finite(s)) return("\u2014")
  h <- floor(s / 3600)
  m <- floor((s %% 3600) / 60)
  ss <- round(s %% 60)
  if (h > 0) sprintf("%dh%02dm", h, m) else sprintf("%dm%02ds", m, ss)
}

# Pace is only shown for foot-based activities, where it is the natural unit.
rmd_fmt_card_stats <- function(distance_km, elapsed_time_s, activity_type) {
  vapply(seq_along(distance_km), function(i) {
    dist <- distance_km[[i]]
    secs <- elapsed_time_s[[i]]
    dur <- rmd_fmt_duration(secs)
    if (!is.finite(dist) || dist == 0) return(dur)
    out <- paste0(sprintf("%.1f km", dist), " \u00b7 ", dur)
    foot <- !is.na(activity_type[[i]]) &&
      activity_type[[i]] %in% c("Run", "Trail Run", "Walk", "Hike")
    if (foot && is.finite(secs)) {
      out <- paste0(out, " \u00b7 ", rmd_fmt_pace((secs / 60) / dist), "/km")
    }
    out
  }, character(1))
}
