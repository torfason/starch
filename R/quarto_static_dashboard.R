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


#' Render the static dashboard index
#'
#' Writes `dashboard_qs/index.html`, a table of every activity in the manifest,
#' linking to those that already have a page. The manifest is written to the
#' staged project as `manifest.csv` and read back by `index.qmd`, so the
#' template holds no knowledge of the repository layout.
#'
#' @param repo Path to the Strava repository.
#' @inheritParams qs_render_activities
#'
#' @return Path to the written page, invisibly.
#' @export
qs_render_index <- function(repo = here("strava_repo"),
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

  if (!quiet) cli::cli_alert_info("Rendering index ({nrow(manifest)} rows)")
  quarto::quarto_render(
    input = as.character(fs::path(stage$qmd, "index.qmd")),
    quiet = !verbose
  )
  qs_collect(stage, out_dir)

  out <- fs::path(out_dir, "index.html")
  if (!quiet) cli::cli_alert_success("Wrote {.file {out}}")
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
  qs_render_index(repo, verbose = verbose, quiet = quiet)
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
