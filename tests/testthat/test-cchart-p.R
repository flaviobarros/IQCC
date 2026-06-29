test_that("cchart.p Phase I Shewhart runs without error", {
  data(binomdata, package = "IQCC")
  attach(binomdata)
  expect_no_error(cchart.p(x1 = Di[1:12], n1 = ni[1:12]))
  detach(binomdata)
})

test_that("cchart.p Phase I Cornish-Fisher runs without error", {
  data(binomdata, package = "IQCC")
  attach(binomdata)
  expect_no_error(cchart.p(x1 = Di[1:12], n1 = ni[1:12], type = "CF",
                           x2 = Di[13:25], n2 = ni[13:25]))
  detach(binomdata)
})

test_that("cchart.p Phase II standardized runs without error", {
  data(binomdata, package = "IQCC")
  attach(binomdata)
  expect_no_error(suppressWarnings(cchart.p(type = "std", p2 = Di[13:25], n2 = ni[13:25], phat = 0.1115833)))
  detach(binomdata)
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
