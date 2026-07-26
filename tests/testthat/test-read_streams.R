
test_that("read_stream can every FIT/GPX/TCX fixture with a valid metadata attribute", {

  skip_if_not_thorough()

  files <- list.files(
    system.file("extdata/activities", package = "starch"),
    pattern = "\\.(gpx|tcx|fit)\\.gz$", full.names = TRUE
  )
  expect_gt(length(files), 0)

  for (f in files) {
    d <- read_stream(f)
    expect_s3_class(d, "tbl_df")
    expect_true("timestamp" %in% names(d))
    expect_s3_class(d$timestamp, "POSIXct")

    meta <- attr(d, "activity_metadata")
    expect_type(meta, "list")
    expect_identical(names(meta), meta_fields)
    # Flat by design: converts to a one-row table.
    expect_identical(dim(tibble::as_tibble(meta)), c(1L, length(meta_fields)))
    if (interactive() && !in_thorough_test_run()) {
      print(d)
      print(attr(d, "activity_metadata") |> tibble::as_tibble())
    }
  }

})

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
  d <- read_stream(fixture_activity("20191117_bike_wahoo_elemnt.fit.gz"))
  expect_s3_class(d, "tbl_df")
  expect_true(all(c("timestamp", "watts") %in% names(d)))
  expect_identical(attr(d, "activity_metadata")$format, "fit")
})

test_that("FIT reader collapses multisport metadata into a slash-joined string", {
  skip_if_not_installed("FITfileR")
  d <- read_stream(fixture_activity("20191117_tri_garmin_fenix_3_hr.fit.gz"))

  meta <- attr(d, "activity_metadata")
  expect_identical(meta$n_sessions, 5L)
  expect_match(meta$sport, "/")
  expect_setequal(
    strsplit(meta$sport, "/")[[1]],
    c("swimming", "transition", "cycling", "running")
  )
})

test_that("FIT workout file yields an empty stream but keeps metadata", {
  skip_if_not_installed("FITfileR")
  d <- read_stream(fixture_workout("Half_Mile_Repeats_workout.fit.gz"))
  # No record messages -> all point columns are dropped as empty.
  expect_equal(ncol(d), 0L)
  expect_identical(attr(d, "activity_metadata")$title, "Half Mile Repeats")
})
