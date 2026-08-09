# The static Quarto stack: one rendered HTML page per activity, the same shape
# as the Rmd stack but built by Quarto. Nothing here is client-side dynamic -
# every page is complete when it is written, and the data lives in the page.
# Contrast quarto_dynamic_dashboard.R, where the pages are empty shells and the
# data arrives as injected scripts.

# Templates are installed read-only, and a Quarto project render writes into
# its own directory, so every render works on a staged copy in a temp dir and
# the results are merged into the repository afterwards.
qs_template_dir <- function() {
  path <- system.file("quarto_static_templates", package = "starch")
  if (!nzchar(path)) {
    stop("Could not locate quarto_static_templates in the installed package.",
      call. = FALSE
    )
  }
  path
}

# The project root is qmd/ and its output-dir is ../html, mirroring the layout
# in inst/quarto_static_templates/_quarto.yml.
qs_stage <- function(envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = envir)
  qmd <- fs::path(root, "qmd")
  html <- fs::path(root, "html")
  fs::dir_copy(qs_template_dir(), qmd)
  fs::dir_create(html)
  list(root = root, qmd = qmd, html = html)
}

# Merge rather than replace: a render only produces the pages it was asked for,
# and site_libs/ is shared, so the target directory accumulates.
qs_collect <- function(stage, out_dir) {
  fs::dir_create(out_dir)
  produced <- fs::dir_ls(stage$html, recurse = TRUE, type = "file")
  rel <- fs::path_rel(produced, stage$html)
  for (i in seq_along(produced)) {
    dest <- fs::path(out_dir, rel[[i]])
    fs::dir_create(fs::path_dir(dest))
    fs::file_copy(produced[[i]], dest, overwrite = TRUE)
  }
  invisible(fs::path(out_dir, rel))
}

# The index shell is plain HTML assembled by R, not a Quarto document, so it
# lives outside the Quarto project directory.
qs_read_shell <- function(name) {
  path <- system.file("quarto_static_shell", name, package = "starch")
  if (!nzchar(path)) {
    stop("Could not locate ", name, " in the installed package.", call. = FALSE)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

qs_fmt_pace <- function(min_per_km) {
  if (!is.finite(min_per_km)) return("\u2014")
  mins <- floor(min_per_km)
  secs <- round((min_per_km - mins) * 60)
  if (secs == 60L) {
    mins <- mins + 1L
    secs <- 0L
  }
  sprintf("%d:%02d", mins, secs)
}

qs_fmt_card_stats <- function(distance_km, elapsed_time_s, activity_type) {
  vapply(seq_along(distance_km), function(i) {
    dist <- distance_km[[i]]
    secs <- elapsed_time_s[[i]]
    dur <- qs_fmt_duration(secs)
    if (!is.finite(dist) || dist == 0) return(dur)
    out <- paste0(sprintf("%.1f km", dist), " \u00b7 ", dur)
    foot <- !is.na(activity_type[[i]]) &&
      activity_type[[i]] %in% c("Run", "Trail Run", "Walk", "Hike")
    if (foot && is.finite(secs)) {
      out <- paste0(out, " \u00b7 ", qs_fmt_pace((secs / 60) / dist), "/km")
    }
    out
  }, character(1))
}

qs_fmt_duration <- function(s) {
  if (!is.finite(s)) return("\u2014")
  h <- floor(s / 3600)
  m <- floor((s %% 3600) / 60)
  ss <- round(s %% 60)
  if (h > 0) sprintf("%d:%02d:%02d", h, m, ss) else sprintf("%d:%02d", m, ss)
}


#' Render per-activity pages with Quarto
#'
#' Renders one HTML page per activity into `dashboard_qs/`, working backwards
#' from the most recent activity and stopping after `max_files`.
#'
#' An activity is rendered when it has a Parquet stream file and does not yet
#' have an HTML page. As in the Rmd stack there is no content hashing: editing
#' the template will not rebuild an existing page. Delete the page to force one.
#'
#' @param repo Path to the Strava repository.
#' @param max_files Maximum number of activities to render in one call.
#' @param max_points Number of stream points kept in each page's charts. The
#'   streams run to tens of thousands of points, which no chart can resolve and
#'   which dominate the size of the rendered page, so they are thinned evenly
#'   on the way in. Statistics are computed from the full stream regardless, so
#'   this affects only chart resolution and file size. Use `0` to keep every
#'   point, which is worth doing for a single activity examined closely.
#' @param verbose Pass the Quarto CLI's own output through, which is verbose
#'   but is the only way to see why a render failed.
#' @param quiet Suppress starch's own progress reporting. Independent of
#'   `verbose`: the default reports progress without the CLI's output.
#'
#' @return A tibble of the activities rendered, invisibly.
#' @export
qs_render_activities <- function(repo = here("strava_repo"),
                                 max_files = 10,
                                 max_points = 600,
                                 verbose = FALSE,
                                 quiet = FALSE) {
  require_pkgs(c(
    "readr", "quarto", "leaflet", "plotly", "ggplot2", "slider", "knitr"
  ))
  require_quarto()

  pq_dir <- fs::path(repo, "activities_parquet")
  out_dir <- fs::path(repo, "dashboard_qs")

  acts <- load_activities_csv(repo)
  acts <- acts[!is.na(acts$stem), ]
  acts$parquet <- fs::path(pq_dir, paste0(acts$stem, ".parquet"))
  acts$html <- fs::path(out_dir, paste0(acts$stem, ".html"))

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

  stage <- qs_stage()
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
    quarto::quarto_render(
      input = as.character(fs::path(stage$qmd, "detail.qmd")),
      output_file = cur_name,
      execute_params = list(
        parquet_path = as.character(fs::path_abs(todo$parquet[[i]])),
        activity_id = todo$activity_id[[i]],
        activity_name = todo$activity_name[[i]],
        activity_type = todo$activity_type[[i]],
        smooth_window = 5,
        max_points = max_points
      ),
      quiet = !verbose
    )
  }

  qs_collect(stage, out_dir)

  if (!quiet) {
    cli::cli_progress_done()
    cli::cli_alert_success("Rendered {n} page{?s} ({elapsed(t0)})")
  }
  invisible(todo)
}


#' Render the plain activity list
#'
#' Writes `dashboard_qs/activity_list.html`, a table of every activity in the
#' manifest, linking to those that already have a page. The manifest is written
#' to the staged project as `manifest.csv` and read back by `list.qmd`, so the
#' template holds no knowledge of the repository layout.
#'
#' This is the simplest of the overview pages, kept alongside the filterable
#' table so the two can be compared.
#'
#' @param repo Path to the Strava repository.
#' @inheritParams qs_render_activities
#'
#' @return Path to the written page, invisibly.
#' @export
qs_render_list <- function(repo = here("strava_repo"),
                           verbose = FALSE,
                           quiet = FALSE) {
  require_pkgs(c("readr", "quarto", "knitr"))
  require_quarto()

  out_dir <- fs::path(repo, "dashboard_qs")
  acts <- load_activities_csv(repo)

  href <- rep("", nrow(acts))
  has_page <- !is.na(acts$stem) &
    file.exists(fs::path(out_dir, paste0(acts$stem, ".html")))
  href[has_page] <- paste0(acts$stem[has_page], ".html")

  manifest <- tibble::tibble(
    date = format(acts$activity_date, "%Y-%m-%d"),
    name = dplyr::coalesce(acts$activity_name, acts$activity_id),
    type = dplyr::coalesce(acts$activity_type, ""),
    distance = ifelse(
      is.finite(acts$distance_km), sprintf("%.2f", acts$distance_km), "\u2014"
    ),
    duration = vapply(acts$elapsed_time_s, qs_fmt_duration, character(1)),
    href = href
  )

  stage <- qs_stage()
  readr::write_csv(manifest, fs::path(stage$qmd, "manifest.csv"))

  if (!quiet) cli::cli_alert_info("Rendering list ({nrow(manifest)} rows)")
  quarto::quarto_render(
    input = as.character(fs::path(stage$qmd, "list.qmd")),
    output_file = "activity_list.html",
    quiet = !verbose
  )
  qs_collect(stage, out_dir)

  out <- fs::path(out_dir, "activity_list.html")
  if (!quiet) cli::cli_alert_success("Wrote {.file {out}}")
  invisible(out)
}


#' Render the filterable activity table
#'
#' Writes `dashboard_qs/overview_table.html`, a sortable and filterable table of
#' every activity in the manifest joined to per-activity statistics read from
#' the Parquet layer. Each row links out to the activity on Strava and, where a
#' page exists, back into the dashboard.
#'
#' Unlike the detail pages this reads every Parquet footer, so it is the slow
#' part of a full build, and the table's data is embedded in the page rather
#' than shared.
#'
#' @param repo Path to the Strava repository.
#' @inheritParams qs_render_activities
#'
#' @return Path to the written page, invisibly.
#' @export
qs_render_table <- function(repo = here("strava_repo"),
                            verbose = FALSE,
                            quiet = FALSE) {
  require_pkgs(c("readr", "quarto", "reactable", "htmltools"))
  require_quarto()

  out_dir <- fs::path(repo, "dashboard_qs")
  pq_dir <- fs::path(repo, "activities_parquet")

  acts <- load_activities_csv(repo)
  acts$parquet <- ifelse(
    is.na(acts$stem), NA_character_,
    as.character(fs::path(pq_dir, paste0(acts$stem, ".parquet")))
  )
  acts$has_page <- !is.na(acts$stem) &
    file.exists(fs::path(out_dir, paste0(acts$stem, ".html")))

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

  stage <- qs_stage()
  data_file <- fs::path(stage$qmd, "table_data.rds")
  saveRDS(tbl, data_file)

  quarto::quarto_render(
    input = as.character(fs::path(stage$qmd, "table.qmd")),
    output_file = "overview_table.html",
    execute_params = list(data_path = as.character(fs::path_abs(data_file))),
    quiet = !verbose
  )
  qs_collect(stage, out_dir)

  out <- fs::path(out_dir, "overview_table.html")
  if (!quiet) {
    cli::cli_alert_success(
      "Overview table written: {nrow(tbl)} row{?s} ({elapsed(t0)})"
    )
  }
  invisible(out)
}


#' Render the dashboard navigation index
#'
#' Writes `dashboard_qs/index.html`, a sidebar of every activity in the
#' manifest over an iframe that shows the selected page. Overview pages are
#' listed above the activities, and only those that exist are offered.
#'
#' The shell is assembled by R rather than rendered by Quarto, so adding an
#' activity rebuilds one small file instead of re-rendering every page, which
#' is what a Quarto-native sidebar would require.
#'
#' @param repo Path to the Strava repository.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the written page, invisibly.
#' @export
qs_render_index <- function(repo = here("strava_repo"), quiet = FALSE) {
  require_pkgs(c("glue", "htmltools"))

  out_dir <- fs::path(repo, "dashboard_qs")
  out <- fs::path(out_dir, "index.html")

  acts <- load_activities_csv(repo)
  has_page <- !is.na(acts$stem) &
    file.exists(fs::path(out_dir, paste0(acts$stem, ".html")))
  page_rel <- ifelse(is.na(acts$stem), NA_character_, paste0(acts$stem, ".html"))

  cards <- glue::glue_data(
    list(
      id = htmltools::htmlEscape(acts$activity_id),
      src = ifelse(has_page, page_rel, ""),
      cls = ifelse(has_page, "card", "card nopage"),
      date_str = format(acts$activity_date, "%Y-%m-%d"),
      time_str = format(acts$activity_date, "%H:%M"),
      name = htmltools::htmlEscape(acts$activity_name),
      type = htmltools::htmlEscape(acts$activity_type),
      stats = qs_fmt_card_stats(
        acts$distance_km, acts$elapsed_time_s, acts$activity_type
      )
    ),
    qs_read_shell("index_card.html"),
    .open = "{{", .close = "}}"
  ) |> paste(collapse = "\n")

  # Overview pages, shown as cards above the activity list. Only those already
  # rendered are offered, so a partial build still produces a working index.
  page_specs <- list(
    list(
      file = "overview_table.html",
      title = "All activities",
      subtitle = "Filterable table"
    ),
    list(
      file = "activity_list.html",
      title = "Activity list",
      subtitle = "Plain listing"
    )
  )
  present <- Filter(function(p) file.exists(fs::path(out_dir, p$file)), page_specs)
  pages_html <- if (length(present) == 0L) {
    ""
  } else {
    card_tpl <- qs_read_shell("index_card_page.html")
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
    qs_read_shell("index.html"),
    .open = "{{", .close = "}}",
    count = nrow(acts),
    n_rendered = sum(has_page),
    pages_html = pages_html,
    cards_html = cards
  )

  fs::dir_create(out_dir)
  writeLines(page, out)
  if (!quiet) {
    cli::cli_alert_success(
      "Index written: {nrow(acts)} activit{?y/ies}, {sum(has_page)} with pages"
    )
  }
  invisible(out)
}


#' Render the static Quarto dashboard
#'
#' Renders outstanding activity pages, then rebuilds the index so that it
#' links to whatever now exists.
#'
#' @inheritParams qs_render_activities
#'
#' @return Path to the index page, invisibly.
#' @export
qs_render_dashboard <- function(repo = here("strava_repo"),
                                max_files = 10,
                                max_points = 600,
                                verbose = FALSE,
                                quiet = FALSE) {
  if (!quiet) cli::cli_h1("Rendering static Quarto dashboard")
  qs_render_activities(
    repo,
    max_files = max_files, max_points = max_points,
    verbose = verbose, quiet = quiet
  )
  qs_render_list(repo, verbose = verbose, quiet = quiet)
  qs_render_table(repo, verbose = verbose, quiet = quiet)
  qs_render_index(repo, quiet = quiet)
}


#' Open the static Quarto dashboard in a browser
#'
#' @param repo Path to the Strava repository.
#'
#' @return Path to the index page, invisibly.
#' @export
qs_view_dashboard <- function(repo = here("strava_repo")) {
  index <- fs::path(repo, "dashboard_qs", "index.html")
  if (!file.exists(index)) {
    stop("No dashboard index yet. Run qs_render_dashboard() first.",
      call. = FALSE
    )
  }
  utils::browseURL(index)
  invisible(index)
}
