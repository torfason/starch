
test_that("GPX reader returns expected point columns and metadata", {
  d <- read_stream(fixture_activity("run_03_garmin.gpx.gz"))
  expect_true(all(c("timestamp", "lat", "lng", "altitude", "heartrate") %in% names(d)))

  meta <- attr(d, "activity_metadata")
  expect_identical(meta$format, "gpx")
  expect_false(is.na(meta$source))
})

test_that("TCX reader captures Sport and device distance", {
  d <- read_stream(fixture_activity("20181108_run_garmin_fenix_3_hr.tcx.gz"))
  expect_true("dev_dist" %in% names(d))

  meta <- attr(d, "activity_metadata")
  expect_identical(meta$format, "tcx")
  expect_identical(meta$sport, "Running")
})

test_that("read_stream errors on an unsupported extension", {
  expect_error(read_stream("activity.xyz"), "unsupported extension")
})

test_that("FIT reader reads points including power", {
  skip_if_not_installed("FITfileR")
  d <- read_stream(fixture_activity("20210218_zwift_bike_race.fit.gz"))
  expect_s3_class(d, "tbl_df")
  expect_true(all(c("timestamp", "watts") %in% names(d)))
  expect_identical(attr(d, "activity_metadata")$format, "fit")
})

test_that("FIT reader collapses multisport metadata into a slash-joined string", {

  # Requires FITfileR and takes a while, only run on thorough
  skip_if_not_installed("FITfileR")
  skip_if_not_thorough()

  d <- read_stream(fixture_activity("20191117_tri_garmin_fenix_3_hr.fit.gz"))
  meta <- attr(d, "activity_metadata")
  expect_identical(meta$n_sessions, 5L)
  expect_match(meta$sport, "/")
  expect_setequal(
    strsplit(meta$sport, "/")[[1]],
    c("swimming", "transition", "cycling", "running")
  )
})

