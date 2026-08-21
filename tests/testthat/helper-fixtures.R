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


# --- Helper function for test values ----

# Compact per-column summary for snapshot tests.
summarize_stream <- function(d) {
  tibble::tibble(
    column = names(d),
    type = sapply(d, typeof),
    n_na = vapply(d, \(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE),
    mean = vapply(d, \(x) if (is.character(x)) NA_real_
                  else mean(as.numeric(x), na.rm = TRUE),
                  numeric(1), USE.NAMES = FALSE),
    hash = vapply(d, \(x) if (is.character(x)) rlang::hash(x)
                  else NA_character_,
                  character(1), USE.NAMES = FALSE)
  )
}

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

# Function to run tests thoroughly - even slow ones
#
# This function is designed to only run as an interactive function, to be
# loaded only when loading package with all test harnesses using ctrl-l.
# To prevent R CMD check from analyzing devtools as a dependency (which
# causes it to hang) devtools::test() is wrapped in eval(pars()).
test_thorough <- function() {
  # Skip unless the caller opted into the slow, exhaustive tests.
  # Wrapped in eval(parse()) to resolve spurious R CMD check issues.
  withr::with_envvar(c(STARCH_TEST_THOROUGH = "true"),
                     eval(parse(text = "devtools::test()")))
}

