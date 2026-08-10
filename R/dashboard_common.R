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

# require_pkgs("quarto") only checks the R wrapper. The wrapper shells out to a
# standalone Quarto binary that is installed separately and is frequently
# absent, in which case the failure surfaces from inside quarto_render() as an
# opaque "Error running quarto CLI from R".
# One progress-bar format for the whole package. cli's default puts the label
# first, so the bar starts in a different column depending on the label's
# length; this keeps it flush left whatever is being counted.
# The line that announces a page is bold across its whole width, so that the
# detail lines under it read as a group. Composed as a plain string first:
# cli's inline markup styles a span, and a span cannot contain the
# substitutions, so the interpolation happens before the styling.
alert_render <- function(msg) {
  cli::cli_alert_info("{.strong {msg}}")
}

bar_format <- function(label) {
  paste0(
    "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} ", label,
    " | {cli::pb_eta}"
  )
}

require_quarto <- function() {
  ok <- tryCatch(!is.null(quarto::quarto_version()), error = function(e) FALSE)
  if (!isTRUE(ok)) {
    stop(
      "The Quarto CLI was not found. It is a standalone program, installed ",
      "separately from the quarto R package: see https://quarto.org.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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
      format = bar_format("reading stream statistics"),
      total = length(paths), clear = FALSE
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
