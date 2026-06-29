test_that("cchart.Xbar1 runs without error on pistonrings data", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.Xbar1(pistonrings[1:25, ], 5))
})

test_that("cchart.Xbar1 returns list with x2bar and sigma", {
  data(pistonrings, package = "IQCC")
  result <- cchart.Xbar1(pistonrings[1:25, ], 5)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_length(result[[1]], 2)
  expect_true(is.numeric(result[[1]][1]))
  expect_true(is.numeric(result[[1]][2]))
  expect_true(result[[1]][2] > 0)
})

test_that("cchart.Xbar2 runs with phase I statistics", {
  data(pistonrings, package = "IQCC")
  stat <- cchart.Xbar1(pistonrings[1:25, ], 5)
  expect_no_error(cchart.Xbar2(pistonrings[26:40, ], stat[[1]][1], stat[[1]][2], 5))
})

test_that("cchart.Xbar Phase I runs without error", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5))
})

test_that("cchart.Xbar Phase II runs without error", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.Xbar(x1 = pistonrings[1:25, ], n1 = 5,
                              x2 = pistonrings[26:40, ], n2 = 5))
})

test_that("cchart.Xbar errors on missing Phase I data", {
  expect_error(cchart.Xbar(), "Phase I data and samples sizes are missing")
})

test_that("cchart.Xbar_R runs without error", {
  data(pistonrings, package = "IQCC")
  expect_no_error(cchart.Xbar_R(pistonrings[1:25, ], 5))
})
