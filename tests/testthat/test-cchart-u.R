test_that("cchart.u Phase I Shewhart runs without error", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  expect_no_error(cchart.u(x1 = yi[1:17], n1 = ni[1:17]))
  detach(moonroof)
})

test_that("cchart.u Phase I Cornish-Fisher runs without error", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  expect_no_error(cchart.u(x1 = yi[1:17], n1 = ni[1:17], type = "CF",
                           x2 = yi[18:34], n2 = ni[18:34]))
  detach(moonroof)
})

test_that("cchart.u Phase II standardized runs without error", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  expect_no_error(suppressWarnings(cchart.u(type = "std", u2 = ui[18:34], n2 = ni[18:34], lambda = 1.4)))
  detach(moonroof)
})

test_that("cchart.u errors on missing Phase I data", {
  expect_error(cchart.u(), "Phase I data and samples sizes are missing")
})

test_that("cchart.u errors on mismatched x1 and n1 lengths", {
  expect_error(cchart.u(x1 = c(1, 2), n1 = c(10, 10, 10)),
               "The arguments x1 and n1 must have the same length")
})
