# A clean GPS stream (lat/lng present, no device distance), sorted by time so
# cumulative columns are guaranteed monotonic regardless of source ordering.
gps_stream <- function() {
  read_stream(fixture_activity("run_03_garmin.gpx.gz")) |> dplyr::arrange(timestamp)
}


test_that("addcols_time adds cumulative elapsed seconds starting at zero", {
  d <- gps_stream() |> addcols_time()
  expect_true("time" %in% names(d))
  expect_type(d$time, "double")
  expect_equal(d$time[1], 0)
  expect_false(is.unsorted(d$time))
})


test_that("addcols_distance adds monotonic cumulative distance", {
  d <- gps_stream() |> addcols_time() |> addcols_distance()
  expect_true("distance" %in% names(d))
  expect_equal(d$distance[1], 0)
  expect_false(is.unsorted(d$distance))
  # GPX carries no device distance, so no QA column is added.
  expect_false("dist_diff" %in% names(d))
})


test_that("addcols_distance adds dist_diff when device distance is present (TCX)", {
  d <- read_stream(fixture_activity("20181108_run_garmin_fenix_3_hr.tcx.gz")) |>
    dplyr::arrange(timestamp) |>
    addcols_time() |>
    addcols_distance()
  expect_true(all(c("distance", "dist_diff") %in% names(d)))
  expect_type(d$dist_diff, "double")
})


test_that("addcols_speed adds smoothed speed with km/h and pace", {
  d <- gps_stream() |> addcols_time() |> addcols_distance() |> addcols_speed(window = 2)
  expect_true(all(c("speed", "speed_kmh", "pace") %in% names(d)))
  ok <- !is.na(d$speed)
  expect_equal(d$speed_kmh[ok], d$speed[ok] * 3.6)
})


test_that("addcols_speed_naive adds cumulative-average speed columns", {
  d <- gps_stream() |> addcols_time() |> addcols_distance() |> addcols_speed_naive()
  expect_true(all(c("speed_ms", "speed_kmh", "pace") %in% names(d)))
  ok <- !is.na(d$speed_ms) & d$time > 0
  expect_equal(d$speed_ms[ok], (d$distance / d$time)[ok])
})


test_that("addcols_splits adds expected columns", {
  d.base <- gps_stream() |>
    addcols_time() |>
    addcols_distance() |>
    addcols_speed()
  d.base |>
    addcols_splits() |>
    names() |>
    expect_contains(c("pace_1k", "pace_5k", "pace_10k"))
  d.base |>
    addcols_splits(type = "time") |>
    names() |>
    expect_contains(c("time_1k", "time_5k", "time_10k"))
})


test_that("addcols_splits structural invariants hold", {
  d <- gps_stream() |>
    addcols_time() |>
    addcols_distance() |>
    addcols_speed() |>
    addcols_speed() |>
    addcols_splits()

  # The longer the split, the lower the pace maximum
  expect_gte(max(d$pace,    na.rm = TRUE), max(d$pace_1k,  na.rm = TRUE))
  expect_gte(max(d$pace_1k, na.rm = TRUE), max(d$pace_5k,  na.rm = TRUE))
  expect_gte(max(d$pace_5k, na.rm = TRUE), max(d$pace_10k, na.rm = TRUE))

  # The longer the split, the higher the pace minimum
  expect_lte(min(d$pace,    na.rm = TRUE), min(d$pace_1k,  na.rm = TRUE))
  expect_lte(min(d$pace_1k, na.rm = TRUE), min(d$pace_5k,  na.rm = TRUE))
  expect_lte(min(d$pace_5k, na.rm = TRUE), min(d$pace_10k, na.rm = TRUE))

  # # The above demonstrated graphically (commented out)
  # d.base |>
  #   addcols_splits() |>
  #   select(distance, starts_with("pace")) |>
  #   tidyr::pivot_longer(-distance) |>
  # ggplot() +
  #   aes(distance, value, color = name) +
  #   geom_line() +
  #   geom_point() +
  #   theme_light()
})


test_that("addcols_splits returns exact values for a constant-pace stream", {
  expected <- (1000 / 3) / 60          # 5.555556 min/km
  d <- tibble(time = as.numeric(0:3600), distance = (0:3600) * 3) |>
    addcols_splits()
  d |>
    dplyr::select(starts_with("pace")) |>
    unlist() |> na.omit() |> as.vector() |>
    expect_all_equal(expected)
})


test_that("addcols_does not regress on existing columns for a representative run", {
  d <- gps_stream() |>
    addcols_time() |>
    addcols_distance() |>
    addcols_speed_naive() |>
    addcols_latlng_offset() |>
    relocate_activity_cols()

  # Structure is exactly reproducible - assert it exactly.
  d |>  expect_named(c("timestamp", "time", "distance", "lat", "lng", "altitude",
      "speed_ms", "speed_kmh", "pace", "heartrate", "lat_offset", "lng_offset" ))

  # Values are not bit-reproducible across platforms - compare with tolerance.
  d |> sapply(typeof) |> expect_snapshot_value(style = "json2")
  d |> summarize_stream() |> expect_snapshot_value(style = "json2")

})
