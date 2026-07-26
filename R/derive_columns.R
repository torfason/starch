# Add derived columns (elapsed time, cumulative distance, speed) to the stream
# tibbles produced by read_stream(). Each function is a pure tibble -> tibble
# transform and expects rows ordered by `timestamp`.

#' Add derived columns to a stream tibble
#'
#' A family of transforms that add elapsed time, cumulative distance, and speed
#' to a stream tibble from [read_stream()]. They are pure `tibble` -> `tibble`
#' functions and assume rows are ordered by `timestamp` (and that `d` is
#' non-empty). Distance and speed additionally require `lat`/`lng`; speed
#' requires that `time` and `distance` already exist, so the usual order is
#' `addcols_time()` -> `addcols_distance()` -> `addcols_speed()`.
#'
#' `addcols_speed()` and `addcols_speed_naive()` are alternatives that both emit
#' `speed_kmh` and `pace`, so apply only one of them.
#'
#' @param d A stream tibble from [read_stream()], ordered by `timestamp`.
#' @param window Half-width, in points, of the centred moving-average window
#'   used to smooth instantaneous speed (`.before`/`.after` in
#'   [slider::slide_dbl()]).
#' @return `d` with the columns described for each function inserted after the
#'   relevant existing column.
#' @examples
#' \dontrun{
#' read_stream("activities/9973795459.gpx.gz") |>
#'   addcols_time() |>
#'   addcols_distance() |>
#'   addcols_speed(window = 2) |>
#'   relocate_activity_columns()
#' }
#' @importFrom dplyr mutate .data
#' @importFrom tibble tibble
#' @name derive_columns
NULL

#' @describeIn derive_columns Add cumulative elapsed `time` (seconds from the
#'   first point).
#' @export
addcols_time <- function(d) {
  segtime <- as.numeric(diff(d$timestamp), units = "secs")
  cumtime <- c(0, cumsum(segtime))
  d |>
    mutate(time = cumtime, .after = "timestamp")
}

#' @describeIn derive_columns Add cumulative `distance` (metres) from geodesic
#'   point-to-point segments. When a device-reported `dev_dist` column is
#'   present (TCX/FIT), also add `dist_diff` = `dev_dist` - `distance` for QA.
#' @export
addcols_distance <- function(d) {
  segdist <- geodist::geodist(
    tibble(lon = d$lng, lat = d$lat),
    sequential = TRUE,
    measure = "geodesic"
  )
  d <- d |>
    mutate(distance = c(0, cumsum(segdist)), .after = "timestamp")

  # dev_dist is present for TCX/FIT but not GPX; only add the QA delta when the
  # column exists. (Replaces the original get0("dev_dist") data-mask lookup.)
  if ("dev_dist" %in% names(d)) {
    d <- d |>
      mutate(dist_diff = .data$dev_dist - .data$distance, .after = "distance")
  }
  d
}

#' @describeIn derive_columns Add smoothed instantaneous `speed` (m/s), plus
#'   `speed_kmh` and `pace` (min/km). Speed is a centred moving average of
#'   point-to-point `diff(distance) / diff(time)` over `window` points either
#'   side.
#' @export
addcols_speed <- function(d, window = 5) {
  d |>
    mutate(
      # Instantaneous speed (m/s), then smoothed in place over the window.
      speed = c(NA_real_, diff(.data$distance)) / c(NA_real_, diff(.data$time)),
      speed = slider::slide_dbl(
        .data$speed, mean, na.rm = TRUE,
        .before = window, .after = window
      ),
      speed_kmh = .data$speed * 3.6,
      pace      = 1000 / (.data$speed * 60),
      .after = "time"
    )
}

#' @describeIn derive_columns Add naive average `speed_ms` (m/s) from cumulative
#'   `distance / time`, plus `speed_kmh` and `pace` (min/km). Unsmoothed; this
#'   is the running average from the start, not an instantaneous rate.
#' @export
addcols_speed_naive <- function(d) {
  d |>
    mutate(
      speed_ms  = .data$distance / .data$time,
      speed_kmh = .data$speed_ms * 3.6,
      pace      = 1000 / (.data$speed_ms * 60),
      .after = "time"
    )
}

# Canonical column order for stream tibbles. relocate_activity_cols() moves
# whichever of these are present to the front, in this order; any column not
# listed is kept, in its existing relative order, after the listed ones.
activity_col_order <- c(
  "timestamp", "time", "distance",                  # axes
  "lat", "lng", "altitude",                         # position
  "speed", "speed_ms", "speed_kmh", "pace",         # movement (robust)
  "heartrate", "cadence", "watts", "temp",          # recorded sensors
  "velocity_smooth", "dev_dist", "grade_smooth",    # device-reported
  "dist_diff"                                       # QA diagnostic
)

#' Reorder stream columns into the canonical activity layout
#'
#' Relocates whichever canonical stream columns are present into a fixed order
#' (axes, position, speed/pace, recorded sensors, device-reported channels, then
#' diagnostics). Uses [dplyr::any_of()], so absent columns are skipped rather
#' than raising an error, and any column not in the canonical list is kept, in
#' its existing relative order, after the listed ones.
#'
#' @param d A stream tibble, e.g. from [read_stream()] after the `addcols_*`
#'   transforms.
#' @return `d` with columns relocated; contents and row order unchanged.
#' @export
relocate_activity_cols <- function(d) {
  dplyr::relocate(d, dplyr::any_of(activity_col_order))
}
