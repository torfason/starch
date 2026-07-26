
test_that("read_stream can every FIT/GPX/TCX fixture with a valid metadata attribute", {

  skip_if_not_thorough()
  skip_if_not_installed("FITfileR")   # loop includes FIT activity fixtures

  files <- fixture_activities()
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

    d.extra_cols <- d |>
      addcols_time() |>
      addcols_distance() |>
      addcols_speed(window = 2) |>
      relocate_activity_cols()

    if (interactive() && !in_thorough_test_run()) {
      cat("\n")
      print(f)
      print(attr(d, "activity_metadata") |> tibble::as_tibble())
      cat("\n")
      print(d)
      cat("\n")
      print(d.extra_cols)
      cat("\n----------------------\n")
    }
  }

})
