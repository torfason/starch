# Quarto dashboard -- a prototype running side by side with the Rmd dashboard
# in dashboard.R. Nothing here is shared with that file beyond the manifest
# reader and the Parquet statistics helper, so either stack can be changed or
# dropped without disturbing the other.
#
# The architecture differs from the Rmd one in a single decisive way: the HTML
# pages carry no data. They are static shells, rendered once by Quarto, and all
# data arrives as `data/*.js` files written by R. Adding activities therefore
# rewrites data files and does not re-render anything. In particular there is
# one detail page for all activities, not one per activity, and it picks its
# activity from the `?id=` query parameter at view time.
#
# Everything in the browser is loaded as a *classic* script, never an ES
# module, and no code path uses fetch(). Both restrictions exist so that the
# dashboard can be opened by double-clicking the file: module scripts and
# fetch() are blocked on file:// URLs. This is also why the pages do not use
# Observable JS, whose Quarto runtime is a module and refuses to start from
# file://.

# Vendored browser libraries. Pinned, because a silently updated Plot would
# change every chart on the next build.
qlib_sources <- list(
  list(
    file = "d3.min.js",
    url = "https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"
  ),
  list(
    file = "plot.umd.min.js",
    url = paste0(
      "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6.17",
      "/dist/plot.umd.min.js"
    )
  )
)

# Point count kept per activity in the detail data files. The streams run to
# tens of thousands of rows and none of the charts can resolve that, so they
# are thinned on write rather than in the browser.
qdetail_points <- 600L

qquarto_dir <- function() {
  path <- system.file("quarto", package = "starch")
  if (!nzchar(path)) {
    stop("Could not locate the quarto directory in the installed package.",
      call. = FALSE
    )
  }
  path
}

# The manifest joined to per-activity Parquet statistics. Deliberately built
# from the same load_activities_csv() and parquet_stream_stats() that the Rmd
# overview table uses, so that the two dashboards cannot disagree on numbers.
qactivities_table <- function(repo, quiet = FALSE) {
  pq_dir <- fs::path(repo, "activities_parquet")

  acts <- load_activities_csv(repo)
  acts$parquet <- ifelse(
    is.na(acts$stem), NA_character_,
    as.character(fs::path(pq_dir, paste0(acts$stem, ".parquet")))
  )
  acts$has_stream <- !is.na(acts$parquet) & file.exists(acts$parquet)

  if (!quiet) {
    cli::cli_alert_info(
      paste0(
        "Reading statistics for {sum(acts$has_stream)} of ",
        "{nrow(acts)} activit{?y/ies}"
      )
    )
  }
  stats <- parquet_stream_stats(acts$parquet[acts$has_stream], quiet = quiet)

  full <- empty_stream_stats()[rep(NA_integer_, nrow(acts)), ]
  if (nrow(stats) > 0L) full[acts$has_stream, ] <- stats
  dplyr::bind_cols(acts, full)
}

# Evenly spaced thinning that always keeps the first and last point, so that
# totals read off the ends of a stream survive the reduction.
qthin <- function(n, keep) {
  if (n <= keep) return(seq_len(n))
  unique(round(seq(1, n, length.out = keep)))
}

# JSON wrapped in an assignment, so the file is a classic script rather than
# something that would have to be fetched.
qwrite_js <- function(x, var, path) {
  json <- jsonlite::toJSON(
    x,
    dataframe = "rows", na = "null", auto_unbox = TRUE,
    digits = 6, POSIXt = "ISO8601", null = "null"
  )
  writeLines(paste0("window.", var, " = ", json, ";"), path, useBytes = TRUE)
}

# The manifest, as consumed by every page.
qwrite_activities_js <- function(tbl, out, quiet = FALSE) {
  data_dir <- fs::path(out, "data")
  fs::dir_create(data_dir)

  keep <- c(
    "activity_id", "activity_name", "activity_type", "activity_date",
    "activity_gear", "commute", "distance_km", "elapsed_time_s",
    "moving_time_s", "elevation_gain_m", "average_speed_ms", "max_speed_ms",
    "average_heart_rate", "max_heart_rate", "average_cadence",
    "average_watts", "max_watts", "calories", "relative_effort",
    "perceived_exertion", "stem", "has_stream", "n_rows", "n_rows_gps",
    "n_rows_dist", "n_rows_speed", "n_rows_hr", "n_rows_alt", "n_rows_cad",
    "n_rows_watts", "stream_columns"
  )
  d <- tbl[, intersect(keep, names(tbl)), drop = FALSE]
  # Emitted as text rather than left to jsonlite's POSIXt handling, which
  # varies with options; the browser parses these with new Date().
  d$activity_date <- format(tbl$activity_date, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  path <- fs::path(data_dir, "activities.js")
  qwrite_js(d, "STARCH_ACTIVITIES", path)
  if (!quiet) {
    cli::cli_alert_success("Manifest written: {nrow(d)} activit{?y/ies}")
  }
  invisible(path)
}

# One data file per activity, thinned. Rebuilt when missing or older than the
# Parquet it derives from; that is a plain mtime test rather than the content
# hashing used by the Parquet layer, because the Parquet files are themselves
# generated and their mtimes are meaningful.
qwrite_detail_js <- function(tbl, out, max_files = 10, quiet = FALSE) {
  data_dir <- fs::path(out, "data")
  fs::dir_create(data_dir)

  usable <- tbl$has_stream & !is.na(tbl$activity_id) & nzchar(tbl$activity_id)
  cand <- tbl[usable, ]
  cand$js <- as.character(fs::path(
    data_dir,
    paste0("act_", gsub("[^A-Za-z0-9_-]", "", cand$activity_id), ".js")
  ))
  stale <- !file.exists(cand$js) |
    file.mtime(cand$parquet) > file.mtime(cand$js)
  todo <- cand[stale, ]
  n <- min(max_files, nrow(todo))

  if (!quiet) {
    cli::cli_alert_info(
      "{nrow(todo)} activit{?y/ies} awaiting data; writing {n}"
    )
  }
  if (n == 0L) return(invisible(todo[0, ]))
  todo <- todo[seq_len(n), ]

  t0 <- Sys.time()
  if (!quiet) {
    cli::cli_progress_bar("Writing activity data", total = n, clear = FALSE)
  }
  for (i in seq_len(n)) {
    qwrite_one_detail(todo[i, ], todo$js[[i]])
    if (!quiet) cli::cli_progress_update()
  }
  if (!quiet) {
    cli::cli_progress_done()
    cli::cli_alert_success("Wrote {n} activity data file{?s} ({elapsed(t0)})")
  }
  invisible(todo)
}

qwrite_one_detail <- function(row, path) {
  d <- nanoparquet::read_parquet(row$parquet[[1]])

  hascol <- function(nm) nm %in% names(d) && any(!is.na(d[[nm]]))
  has_dist <- nrow(d) > 0 && hascol("distance")
  has_time <- nrow(d) > 0 && hascol("time")

  pt_cols <- intersect(
    c("time", "distance", "altitude", "heartrate", "pace", "lat", "lng"),
    names(d)
  )
  idx <- qthin(nrow(d), qdetail_points)
  pts <- if (length(pt_cols) == 0L || nrow(d) == 0L) {
    data.frame()
  } else {
    p <- as.data.frame(d[idx, pt_cols, drop = FALSE])
    # Pace explodes towards infinity when stationary; the Rmd template
    # dead-bands it on speed, and the same guard is applied here, before the
    # data leaves R.
    if ("pace" %in% names(p) && "speed" %in% names(d)) {
      p$pace[d$speed[idx] < 0.3] <- NA_real_
    }
    if ("pace" %in% names(p)) p$pace[!is.finite(p$pace)] <- NA_real_
    p
  }

  total_dist_km <- if (has_dist) {
    max(d$distance, na.rm = TRUE) / 1000
  } else NA_real_
  total_time_s <- if (has_time) max(d$time, na.rm = TRUE) else NA_real_

  payload <- list(
    activity_id = row$activity_id[[1]],
    stem = row$stem[[1]],
    has_distance = has_dist,
    total_distance_km = total_dist_km,
    total_time_s = total_time_s,
    avg_pace_min_km = if (isTRUE(total_dist_km > 0) &&
      is.finite(total_time_s)) {
      (total_time_s / 60) / total_dist_km
    } else NA_real_,
    avg_heartrate = if (hascol("heartrate")) {
      mean(d$heartrate, na.rm = TRUE)
    } else NA_real_,
    elevation_gain_m = if (hascol("altitude")) {
      sum(pmax(0, diff(d$altitude)), na.rm = TRUE)
    } else NA_real_,
    n_points_full = nrow(d),
    points = pts
  )

  json <- jsonlite::toJSON(payload, dataframe = "rows", na = "null",
    auto_unbox = TRUE, digits = 6, null = "null")
  writeLines(
    paste0(
      "window.STARCH_DETAIL = window.STARCH_DETAIL || {};\n",
      "window.STARCH_DETAIL[",
      jsonlite::toJSON(row$activity_id[[1]], auto_unbox = TRUE),
      "] = ", json, ";"
    ),
    path, useBytes = TRUE
  )
  invisible(path)
}

# d3 and Plot are downloaded once into the output directory rather than
# vendored into the package, which keeps ~500 KB of minified JavaScript out of
# the sources. The build therefore needs the network the first time; the
# resulting dashboard never does.
qvendor_libs <- function(out, quiet = FALSE) {
  lib_dir <- fs::path(out, "lib")
  fs::dir_create(lib_dir)

  for (spec in qlib_sources) {
    dest <- fs::path(lib_dir, spec$file)
    if (file.exists(dest) && file.size(dest) > 0) next
    if (!quiet) cli::cli_alert_info("Downloading {.file {spec$file}}")
    ok <- tryCatch(
      {
        utils::download.file(spec$url, dest, quiet = TRUE, mode = "wb")
        file.exists(dest) && file.size(dest) > 0
      },
      error = function(e) FALSE
    )
    if (!ok) {
      stop(
        "Could not download ", spec$file, " from\n  ", spec$url,
        "\nThe dashboard needs it to draw charts. Download it by hand into\n  ",
        lib_dir,
        call. = FALSE
      )
    }
  }

  # Our own assets are copied every time, so that editing them in the package
  # sources is picked up without having to clear the output directory.
  static <- fs::path(qquarto_dir(), "_static")
  for (f in c("starch-dash.js", "starch-dash.css")) {
    fs::file_copy(fs::path(static, f), fs::path(lib_dir, f), overwrite = TRUE)
  }
  invisible(lib_dir)
}

# The Quarto project is staged into a temporary directory because the copy in
# the installed package is read-only and a project render writes into its own
# source tree. Rendering is skipped when the pages already exist and no
# template is newer, since the pages hold no data and only change when the
# templates do.
qrender_pages <- function(out, force = FALSE, quiet = FALSE) {
  require_pkgs("quarto")

  src <- qquarto_dir()
  pages <- c("dash_overview.html", "detail_a.html")
  targets <- fs::path(out, pages)
  newest_src <- suppressWarnings(max(file.mtime(
    fs::dir_ls(src, recurse = TRUE, type = "file")
  )))
  fresh <- all(file.exists(targets)) &&
    isTRUE(min(file.mtime(targets)) > newest_src)

  if (!force && fresh) {
    if (!quiet) cli::cli_alert_info("Pages are current; skipping Quarto render")
  } else {
    t0 <- Sys.time()
    if (!quiet) cli::cli_alert_info("Rendering Quarto project")
    stage <- withr::local_tempdir()
    fs::dir_copy(src, fs::path(stage, "quarto"))
    proj <- fs::path(stage, "quarto")
    # The pages reference lib/starch-dash.css at render time, so the project
    # needs a copy of it even though the real one is written to the output.
    fs::dir_create(fs::path(proj, "lib"))
    fs::file_copy(
      fs::path(src, "_static", "starch-dash.css"),
      fs::path(proj, "lib", "starch-dash.css"),
      overwrite = TRUE
    )
    quarto::quarto_render(input = as.character(proj), quiet = FALSE)

    site <- fs::path(proj, "_site")
    if (!fs::dir_exists(site)) {
      stop(
        "Quarto render produced no _site directory in:\n  ", proj,
        call. = FALSE
      )
    }
    # Merged in file by file rather than by replacing directories wholesale.
    # Quarto also emits lib/ (it copies the stylesheet the pages reference as
    # a project resource), and deleting that directory would take the
    # downloaded d3 and Plot builds with it.
    fs::dir_create(out)
    for (f in fs::dir_ls(site, recurse = TRUE, type = "file")) {
      dest <- fs::path(out, fs::path_rel(f, site))
      fs::dir_create(fs::path_dir(dest))
      fs::file_copy(f, dest, overwrite = TRUE)
    }
    # Quarto writes an index.html redirecting to the first document in the
    # project. The real entry point is the index shell, so point it there.
    writeLines(
      c(
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
        "<meta http-equiv=\"refresh\" content=\"0; url=dash_index.html\">",
        "</head><body></body></html>"
      ),
      fs::path(out, "index.html")
    )
    if (!quiet) cli::cli_alert_success("Pages rendered ({elapsed(t0)})")
  }

  # The index is a plain static shell rather than a Quarto document: it is a
  # full-viewport frameset, which the Quarto page chrome would fight.
  fs::file_copy(
    fs::path(src, "_static", "dash_index.html"),
    fs::path(out, "dash_index.html"),
    overwrite = TRUE
  )
  invisible(out)
}

#' Render the Quarto dashboard
#'
#' Builds `qdashboard/` inside the Strava repository: a static index shell, an
#' overview page carrying a client-side trends chart and activity table, and a
#' single detail page that renders whichever activity is named by its `?id=`
#' query parameter.
#'
#' The pages carry no data of their own. Everything they display is written to
#' `qdashboard/data/` as JavaScript files, so adding activities rewrites data
#' and does not re-render any HTML. The result opens from disk without a web
#' server, and needs no network once built.
#'
#' This runs alongside [render_dashboard()], which builds the older Rmd
#' dashboard into `dashboard/`. The two share the manifest reader and the
#' Parquet statistics helper and are otherwise independent.
#'
#' @param repo Path to the Strava repository.
#' @param max_files Maximum number of per-activity data files to write in one
#'   call.
#' @param force Re-render the Quarto pages even when they look current.
#' @param quiet Suppress progress reporting.
#'
#' @return Path to the dashboard index, invisibly.
#' @export
qrender_dashboard <- function(repo = here("strava_repo"),
                              max_files = 10,
                              force = FALSE,
                              quiet = FALSE) {
  require_pkgs(c("readr", "jsonlite", "quarto"))

  t_all <- Sys.time()
  out <- fs::path(repo, "qdashboard")
  if (!quiet) {
    cli::cli_h1("Rendering Quarto dashboard")
    cli::cli_alert_info("Repository {.path {repo}}")
  }

  fs::dir_create(out)
  qvendor_libs(out, quiet = quiet)
  tbl <- qactivities_table(repo, quiet = quiet)
  qwrite_activities_js(tbl, out, quiet = quiet)
  qwrite_detail_js(tbl, out, max_files = max_files, quiet = quiet)
  qrender_pages(out, force = force, quiet = quiet)

  index <- fs::path(out, "dash_index.html")
  if (!quiet) cli::cli_alert_success("Done ({elapsed(t_all)})")
  invisible(index)
}

#' Open the Quarto dashboard in a browser
#'
#' @param repo Path to the Strava repository.
#'
#' @return Path to the index, invisibly.
#' @export
qview_dashboard <- function(repo = here("strava_repo")) {
  index <- fs::path(repo, "qdashboard", "dash_index.html")
  if (!file.exists(index)) {
    stop(
      "No dashboard index at:\n  ", index, "\nRun qrender_dashboard() first.",
      call. = FALSE
    )
  }
  utils::browseURL(as.character(fs::path_abs(index)))
  invisible(index)
}
