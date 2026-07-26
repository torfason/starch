
# Function to run tests thoroughly - even slow ones
test_thorough <- function() {
  # Skip unless the caller opted into the slow, exhaustive tests.
  withr::with_envvar(c(STARCH_TEST_THOROUGH = "true"), devtools::test())
}
