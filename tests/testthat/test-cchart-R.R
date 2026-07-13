test_that("cchart.R with norm type returns a qcc object", {
  data(pistonrings, package = "IQCC")
  chart <- cchart.R(pistonrings[1:25, ], 5, type = "norm")
  expect_s3_class(chart, "qcc")
})

test_that("cchart.R with tukey type returns a qcc object", {
  data(pistonrings, package = "IQCC")
  chart <- cchart.R(
    pistonrings[26:40, ],
    5,
    type = "tukey",
    y = pistonrings[1:25, ]
  )
  expect_s3_class(chart, "qcc")
})

test_that("cchart.R validates its design contract", {
  data(pistonrings, package = "IQCC")
  expect_error(cchart.R(pistonrings[1:25, ], 1), "greater than or equal to 2")
  expect_error(cchart.R(pistonrings[1:25, ], 5, type = "other"), "arg")
  expect_error(
    cchart.R(pistonrings[1:25, ], 5, type = "tukey"),
    "y must be supplied"
  )
})
