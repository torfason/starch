# The parallel branch needs daemons, which cost a second or two to start and
# stop, so it is set up once and torn down at the end rather than per test.
skip_without_daemons <- function() {
  skip_if_not_installed("mirai")
  skip_if_not(starch_parallel(), "no mirai daemons running")
}


test_that("starch_map() is sequential when no daemons are running", {
  skip_if(
    requireNamespace("mirai", quietly = TRUE) && starch_parallel(),
    "daemons already running"
  )
  expect_false(starch_parallel())
  expect_identical(
    starch_map(1:4, function(i) i * 2, .quiet = TRUE),
    list(2, 4, 6, 8)
  )
})


test_that("starch_map() passes extra arguments and handles empty input", {
  expect_identical(
    starch_map(1:3, function(i, k) i + k, k = 10, .quiet = TRUE),
    list(11, 12, 13)
  )
  expect_identical(starch_map(list(), identity, .quiet = TRUE), list())
  expect_identical(starch_map(character(0), identity, .quiet = TRUE), list())
})


test_that("starch_map() restores the progress option it overrides", {
  # It sets cli.progress_show_after to 0 so that short bars appear at all.
  withr::local_options(cli.progress_show_after = 7)
  starch_map(1:2, identity, .quiet = TRUE)
  expect_identical(getOption("cli.progress_show_after"), 7)

  # Including when the worker throws.
  expect_error(starch_map(1:2, function(i) stop("boom"), .quiet = TRUE))
  expect_identical(getOption("cli.progress_show_after"), 7)
})


test_that("starch_map() fails fast, naming the element that failed", {
  expect_error(
    starch_map(1:4, function(i) if (i == 3) stop("boom") else i, .quiet = TRUE),
    "boom"
  )
})


test_that("gzip_one() compresses one file and reports only success", {
  dir <- withr::local_tempdir()
  path <- fs::path(dir, "9973795459.gpx")
  writeLines(rep("<trkpt lat='64.1' lon='-21.9'/>", 200), path)

  expect_true(gzip_one(path))
  expect_false(fs::file_exists(path))
  expect_true(fs::file_exists(paste0(path, ".gz")))
  expect_length(readLines(paste0(path, ".gz")), 200L)
})


test_that("starch_map() gives the same answers in parallel as sequentially", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::defer(mirai::daemons(0))
  mirai::daemons(2)
  skip_without_daemons()

  expect_identical(
    starch_map(1:6, function(i) i * 2, .quiet = TRUE),
    list(2, 4, 6, 8, 10, 12)
  )
  expect_identical(
    starch_map(1:3, function(i, k) i + k, k = 10, .quiet = TRUE),
    list(11, 12, 13)
  )
  expect_identical(starch_map(list(), identity, .quiet = TRUE), list())

  # A worker that is a package function has to survive being sent to another
  # process, where starch is loaded from the library rather than inherited.
  expect_identical(
    unlist(starch_map(list("a/b.txt", "c/d.txt"), fs::path_file,
                      .quiet = TRUE)),
    c("b.txt", "d.txt")
  )

  # Fail-fast holds in parallel too, and the message survives the round trip.
  expect_error(
    starch_map(1:4, function(i) if (i == 3) stop("boom") else i, .quiet = TRUE),
    "boom"
  )
})


test_that("tracks compress correctly in parallel", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::defer(mirai::daemons(0))
  mirai::daemons(2)
  skip_without_daemons()

  dir <- withr::local_tempdir()
  paths <- fs::path(dir, sprintf("act_%03d.gpx", 1:12))
  for (p in paths) writeLines(rep("<trkpt lat='64.1' lon='-21.9'/>", 100), p)

  out <- starch_map(paths, gzip_one, .quiet = TRUE)
  expect_true(all(unlist(out)))
  expect_false(any(fs::file_exists(paths)))
  expect_true(all(fs::file_exists(paste0(paths, ".gz"))))
})
