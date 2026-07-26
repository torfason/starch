# Shared test helpers (auto-sourced by testthat before tests run).

# Absolute path to a vendored fixture activity in inst/extdata.
fixture_activity <- function(name) {
  system.file("extdata/activities", name, package = "starch", mustWork = TRUE)
}

# Absolute path to a vendored fixture activity in inst/extdata.
fixture_workout <- function(name) {
  system.file("extdata/workouts", name, package = "starch", mustWork = TRUE)
}

# Absolute paths to all vendored activity fixtures.
fixture_activities <- function() {
  list.files(
    system.file("extdata/activities", package = "starch"),
    pattern = "\\.(gpx|tcx|fit)\\.gz$", full.names = TRUE
  )
}

# Absolute paths to all vendored workout fixtures.
fixture_workouts <- function() {
  list.files(
    system.file("extdata/workouts", package = "starch"),
    pattern = "\\.(gpx|tcx|fit)\\.gz$", full.names = TRUE
  )
}


# The fields every activity_metadata record must expose, in order.
meta_fields <- c(
  "format", "source", "sport", "sub_sport", "title", "start_time",
  "n_sessions", "total_distance", "total_timer_time", "total_calories"
)



# --- Managing thorough test runs ---


# Simple boolean to check the env var for including the slow, exhaustive tests.
in_thorough_test_run <- function(){
  identical(tolower(Sys.getenv("STARCH_TEST_THOROUGH")), "true")
}

# Skip unless the caller opted into the slow, exhaustive tests.
skip_if_not_thorough <- function() {
  if (!in_thorough_test_run()) {
    skip("thorough tests skipped - force a run with test_thorough()")
  }
}

