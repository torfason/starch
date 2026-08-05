# Add derived columns (elapsed time, cumulative distance, speed) to the stream
# tibbles produced by read_stream(). Each function is a pure tibble -> tibble
# transform and expects rows ordered by `timestamp`.

#' Add derived columns to a stream tibble
#'
#' A family of transforms that add elapsed time, cumulative distance, speed, and
#' recentred coordinates to a stream tibble from [read_stream()]. They are pure
#' `tibble` -> `tibble`
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
#'   relevant existing column, or appended where there is no such column.
#' @examples
#' \dontrun{
#' read_stream("activities/9973795459.gpx.gz") |>
#'   addcols_time() |>
#'   addcols_distance() |>
#'   addcols_speed(window = 2) |>
#'   relocate_activity_cols()
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
  # Distance is accumulated over points that actually have coordinates. Feeding
  # NA lat/lng straight into geodist() yields NA segments, and cumsum() then
  # propagates the first NA to every later row - so a single missing fix would
  # wipe out cumulative distance for the rest of the track. Instead, take the
  # geodesic between successive *valid* points (a straight line bridges each
  # gap) and leave the missing-coordinate rows themselves as NA.
  valid <- !is.na(d$lat) & !is.na(d$lng)
  cumdist <- rep(NA_real_, nrow(d))
  if (sum(valid) >= 2) {
    segdist <- geodist::geodist(
      tibble(lon = d$lng[valid], lat = d$lat[valid]),
      sequential = TRUE,
      measure = "geodesic"
    )
    cumdist[valid] <- c(0, cumsum(segdist))
  } else if (sum(valid) == 1) {
    cumdist[valid] <- 0
  }
  d <- d |>
    mutate(distance = cumdist, .after = "timestamp")

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

#' @describeIn derive_columns Add `lat_offset` and `lng_offset` (degrees):
#'   `lat`/`lng` relative to the first point that has both. Absolute
#'   coordinates dwarf the within-activity variation, so the offsets make
#'   movement legible when eyeballing a stream.
#' @export
addcols_latlng_offset <- function(d) {
  # Anchor on the first point carrying both coordinates, so a leading gap in
  # the fix does not send the whole track to NA. Both offsets share one anchor
  # point, so they stay a consistent origin rather than two independent ones.
  anchored <- !is.na(d$lat) & !is.na(d$lng)
  lat0 <- d$lat[anchored][1]
  lng0 <- d$lng[anchored][1]

  d |>
    mutate(
      lat_offset = .data$lat - lat0,
      lng_offset = .data$lng - lng0
    )
}


#' Add rolling split columns
#'
#' Adds one column per window in `distance`, each holding a rolling measure
#' over the preceding window: either the pace across it, or the time taken to
#' cover it. Every value looks strictly backwards, so `min(pace_10k)` over an
#' activity is the fastest 10 km run within it.
#'
#' The point exactly one window back almost never coincides with a recorded
#' sample, so the time at that distance is interpolated rather than snapped to
#' the nearest row. Values are `NA` over the opening stretch, where the window
#' would reach back beyond the start of the track.
#'
#' @section Column names and units:
#' Names are the type, the window, and the unit suffix: `pace_1k`, `time_5k`,
#' `pace_400m`, `pace_5mi`. Fractional windows replace the decimal point with
#' an underscore, so 21.1 km becomes `pace_21_1k`, which needs no backticks.
#'
#' `type = "pace"` is reported per kilometre for `"km"` and `"m"`, and per mile
#' for `"miles"`, so a 400 m split reads as min/km rather than the useless
#' min/metre. This matches the units of the `pace` column from
#' [addcols_speed()], so the two can share an axis. `type = "time"` is in
#' seconds, matching the `time` column.
#'
#' Windows longer than the activity produce nothing, and those columns are
#' dropped rather than carried as all-`NA`. Columns are appended in the order
#' given; use [relocate_activity_cols()] if a canonical order is wanted.
#'
#' @param d A stream tibble carrying `distance` and `time`. If either is
#'   absent, or there are too few distinct distances to interpolate between,
#'   `d` is returned unchanged.
#' @param distance Window sizes, in `units`.
#' @param units Unit the windows are expressed in.
#' @param type Whether each column holds the pace across its window or the time
#'   taken to cover it.
#'
#' @return `d` with one column appended per window that yielded any data.
#' @export
addcols_splits <- function(d,
                           distance = c(1, 5, 10),
                           units = c("km", "m", "miles"),
                           type = c("pace", "time")) {

  # Metres per unit, and the suffix each contributes to generated column names.
  split_unit_m <- c(km = 1000, m = 1, miles = 1609.344)
  split_unit_suffix <- c(km = "k", m = "m", miles = "mi")

  # Verify inputs
  units <- match.arg(units)
  type <- match.arg(type)
  stopifnot(is.numeric(distance), length(distance) > 0L, all(distance > 0))

  window_m <- distance * split_unit_m[[units]]
  pace_unit_m <- if (units == "miles") {
    split_unit_m[["miles"]]
  } else {
    split_unit_m[["km"]]
  }

  # as.character() rather than format(), which pads a vector to a common width
  # and would render 1 as "1.0" whenever 21.1 appears alongside it.
  labels <- sub("[.]", "_", as.character(distance))
  nms <- paste0(type, "_", labels, split_unit_suffix[[units]])
  if (!identical(nms, make.names(nms))) {
    stop(
      "Window sizes produce non-syntactic column names: ",
      paste(nms[nms != make.names(nms)], collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(c("distance", "time") %in% names(d))) return(d)
  ok <- !is.na(d$distance) & !is.na(d$time)
  # Interpolation needs two distinct distances; a stream whose distance never
  # advances has none.
  if (sum(ok) < 2L || length(unique(d$distance[ok])) < 2L) return(d)

  for (i in seq_along(window_m)) {
    # rule = 1 returns NA where the window reaches back beyond the start of the
    # track, which is what leaves the opening stretch empty. ties = min
    # resolves stationary spells to the first time a distance was reached,
    # which is how a split is conventionally read, and specifying it also
    # suppresses the tie-collapsing warning on any activity with a pause.
    t_back <- stats::approx(
      x = d$distance[ok],
      y = d$time[ok],
      xout = d$distance - window_m[[i]],
      rule = 1,
      ties = min
    )$y
    dt <- d$time - t_back
    v <- if (type == "pace") {
      (dt / 60) / (window_m[[i]] / pace_unit_m)
    } else {
      dt
    }
    if (!all(is.na(v))) d[[nms[[i]]]] <- v
  }
  d
}


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

  # Canonical column order for stream tibbles. relocate_activity_cols() moves
  # whichever of these are present to the front, in this order; any column not
  # listed is kept, in its existing relative order, after the listed ones.
  activity_col_order <- c(
    "timestamp", "time", "distance",                  # axes
    "lat", "lng", "altitude",                         # position
    "speed", "speed_ms", "speed_kmh", "pace",         # movement (robust)
    "heartrate", "cadence", "watts", "temp",          # recorded sensors
    "velocity_smooth", "dev_dist", "grade_smooth",    # device-reported
    "dist_diff",                                      # QA diagnostic
    "lat_offset", "lng_offset"                        # recentred position
  )

  # Relocate to activity_col_order using dplyr::relocate()
  dplyr::relocate(d, dplyr::any_of(activity_col_order))
}
