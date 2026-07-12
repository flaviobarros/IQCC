test_that("cchart.p Phase I Shewhart runs without error", {
  data(binomdata, package = "IQCC")
  expect_no_error(cchart.p(x1 = binomdata$Di[1:12],
                           n1 = binomdata$ni[1:12]))
})

test_that("cchart.p Phase I Cornish-Fisher runs without error", {
  data(binomdata, package = "IQCC")
  expect_no_error(cchart.p(x1 = binomdata$Di[1:12],
                           n1 = binomdata$ni[1:12], type = "CF",
                           x2 = binomdata$Di[13:25],
                           n2 = binomdata$ni[13:25]))
})

test_that("cchart.p Phase II standardized runs without error", {
  data(binomdata, package = "IQCC")
  expect_no_error(cchart.p(type = "std",
                           p2 = binomdata$Di[13:25] / binomdata$ni[13:25],
                           n2 = binomdata$ni[13:25], phat = 0.1115833))
})

test_that("cchart.p errors on missing Phase I data", {
  expect_error(cchart.p(), "Phase I data and samples sizes are missing")
})

test_that("cchart.p errors on mismatched x1 and n1 lengths", {
  expect_error(cchart.p(x1 = c(1, 2), n1 = c(1, 2, 3)),
               "The arguments x1 and n1 must have the same length")
})

test_that("cchart.p errors on missing Phase II sizes", {
  expect_error(cchart.p(x1 = c(1, 2), n1 = c(10, 10), x2 = c(1, 2)),
               "Phase II samples sizes not specified")
})

test_that("cchart.p with p1 input path runs without error", {
  data(binomdata, package = "IQCC")
  p1_vals <- binomdata$Di[1:12] / binomdata$ni[1:12]
  expect_no_error(cchart.p(p1 = p1_vals, n1 = binomdata$ni[1:12]))
})

test_that("cchart.p Phase II with x2 runs without error", {
  data(binomdata, package = "IQCC")
  expect_no_error(cchart.p(x1 = binomdata$Di[1:12],
                           n1 = binomdata$ni[1:12],
                           x2 = binomdata$Di[13:25],
                           n2 = binomdata$ni[13:25]))
})

test_that("cchart.p Phase II length mismatch errors", {
  expect_error(cchart.p(x1 = c(1, 2), n1 = c(10, 10),
                        x2 = c(1, 2, 3), n2 = c(10, 10)),
               "The arguments x2 and n2 must have the same length")
})

test_that("cchart.p Phase II with phat derived from Phase I", {
  data(binomdata, package = "IQCC")
  expect_no_error(cchart.p(x1 = binomdata$Di[1:12],
                           n1 = binomdata$ni[1:12],
                           x2 = binomdata$Di[13:25],
                           n2 = binomdata$ni[13:25]))
})
