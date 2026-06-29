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

test_that("cchart.u with u1 input path runs without error", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  u1_vals <- yi[1:17] / ni[1:17]
  expect_no_error(cchart.u(u1 = u1_vals, n1 = ni[1:17]))
  detach(moonroof)
})

test_that("cchart.u Phase II with x2 runs without error", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  expect_no_error(cchart.u(x1 = yi[1:17], n1 = ni[1:17],
                           x2 = yi[18:34], n2 = ni[18:34]))
  detach(moonroof)
})

test_that("cchart.u Phase II length mismatch errors", {
  expect_error(cchart.u(x1 = c(1, 2), n1 = c(10, 10),
                        x2 = c(1, 2, 3), n2 = c(10, 10)),
               "The arguments x2 and n2 must have the same length")
})

test_that("cchart.u Phase II with lambda derived from Phase I", {
  data(moonroof, package = "IQCC")
  attach(moonroof)
  expect_no_error(cchart.u(x1 = yi[1:17], n1 = ni[1:17],
                           x2 = yi[18:34], n2 = ni[18:34]))
  detach(moonroof)
})
