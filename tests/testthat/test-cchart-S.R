test_that("cchart.S with normalized limits returns a qcc object", {
  data(softdrink, package = "IQCC")
  chart <- cchart.S(softdrink, type = "n")
  expect_s3_class(chart, "qcc")
})

test_that("cchart.S with exact limits returns a qcc object", {
  data(softdrink, package = "IQCC")
  chart <- cchart.S(softdrink, type = "e", m = 10)
  expect_s3_class(chart, "qcc")
})

test_that("cchart.S exact limits warn and fall back when m is missing", {
  data(softdrink, package = "IQCC")
  expect_warning(
    chart <- cchart.S(softdrink, type = "e"),
    "sample size m wasn't specified"
  )
  expect_s3_class(chart, "qcc")
})

test_that("cchart.S validates its design contract", {
  data(softdrink, package = "IQCC")
  expect_error(cchart.S(softdrink, type = "other"), "arg")
  expect_error(cchart.S(softdrink, type = "e", m = 1),
               "greater than or equal to 2")
})
