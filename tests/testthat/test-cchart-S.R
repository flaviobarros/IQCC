test_that("cchart.S with normalized limits runs without error", {
  data(softdrink, package = "IQCC")
  expect_no_error(cchart.S(softdrink, type = "n"))
})

test_that("cchart.S with exact limits runs without error", {
  data(softdrink, package = "IQCC")
  expect_no_error(cchart.S(softdrink, type = "e", 10))
})

test_that("cchart.S with exact limits warns when m is missing", {
  data(softdrink, package = "IQCC")
  expect_warning(cchart.S(softdrink, type = "e"), "sample size m wasn't specified")
})
