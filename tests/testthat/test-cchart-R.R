test_that("cchart.R with norm type runs without error", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.R(pistonrings[1:25, ], 5, type = "norm"))
})

test_that("cchart.R with tukey type runs without error", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.R(pistonrings[26:40, ], 5, type = "tukey", pistonrings[1:25, ]))
})
