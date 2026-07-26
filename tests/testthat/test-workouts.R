# Workout files are plans, not activities (no record/session/timestamp data).
# read_stream() should return an empty (0 x 0) stream, still carrying metadata.

test_that("workout files read as an empty (0 x 0) stream with metadata", {
  skip_if_not_installed("FITfileR")

  files <- fixture_workouts()
  expect_gt(length(files), 0)

  for (f in files) {
    d <- read_stream(f)
    expect_s3_class(d, "tbl_df")
    expect_identical(dim(d), c(0L, 0L))

    meta <- attr(d, "activity_metadata")
    expect_type(meta, "list")
    expect_identical(names(meta), meta_fields)
    expect_identical(dim(tibble::as_tibble(meta)), c(1L, length(meta_fields)))
  }
})

test_that("workout metadata captures the workout name", {
  skip_if_not_installed("FITfileR")
  d <- read_stream(fixture_workout("Half_Mile_Repeats_workout.fit.gz"))

  meta <- attr(d, "activity_metadata")
  expect_identical(meta$format, "fit")
  expect_identical(meta$title, "Half Mile Repeats")
})
