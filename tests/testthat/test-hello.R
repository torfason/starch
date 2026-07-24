
# Hello test functions
test_that("hello works", {
  hello() |> expect_equal("Hello starch!")
})
