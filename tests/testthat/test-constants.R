test_that("d2 returns correct values for known inputs", {
  expect_equal(d2(2), 1.128379, tolerance = 1e-5)
  expect_equal(d2(5), 2.325929, tolerance = 1e-5)
  expect_equal(d2(10), 3.077505, tolerance = 1e-5)
})

test_that("d2 works with vector input", {
  result <- d2(2:5)
  expect_length(result, 4)
  expect_equal(result[1], d2(2))
  expect_equal(result[2], d2(3))
  expect_equal(result[3], d2(4))
  expect_equal(result[4], d2(5))
})

test_that("d2 rejects n < 2", {
  expect_error(d2(1), "n must be >= 2")
  expect_error(d2(0), "n must be >= 2")
  expect_error(d2(c(1, 5)), "n must be >= 2")
})

test_that("d3 returns correct values for known inputs", {
  expect_equal(d3(2), 0.8525025, tolerance = 1e-5)
  expect_equal(d3(5), 0.8640819, tolerance = 1e-5)
})

test_that("d3 works correctly with vector input (regression test for d2[i] bug)", {
  result <- d3(2:5)
  expect_length(result, 4)
  expect_equal(result[1], d3(2), tolerance = 1e-5)
  expect_equal(result[2], d3(3), tolerance = 1e-5)
  expect_equal(result[3], d3(4), tolerance = 1e-5)
  expect_equal(result[4], d3(5), tolerance = 1e-5)
})

test_that("c4 returns correct values for known inputs", {
  expect_equal(c4(2), 0.7978846, tolerance = 1e-5)
  expect_equal(c4(5), 0.9399856, tolerance = 1e-5)
  expect_equal(c4(10), 0.9726593, tolerance = 1e-5)
})

test_that("c4 rejects n < 2", {
  expect_error(c4(1), "n must be >= 2")
})

test_that("table.const returns matrix with correct dimensions", {
  result <- table.const(10)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 9)
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("d2", "d3", "c4"))
  expect_equal(rownames(result), as.character(2:10))
})

test_that("table.const values match individual constant functions", {
  result <- table.const(5)
  expect_equal(unname(result[, "d2"]), d2(2:5))
  expect_equal(unname(result[, "d3"]), d3(2:5))
  expect_equal(unname(result[, "c4"]), c4(2:5))
})

test_that("alpha.risk returns values > 0.0027 for small n", {
  result <- alpha.risk(5)
  expect_true(result > 0.0027)
})

test_that("alpha.risk works with vector input", {
  result <- alpha.risk(c(5, 10, 15))
  expect_length(result, 3)
  expect_true(all(result > 0))
})

test_that("table.qtukey returns matrix with 4 columns", {
  result <- table.qtukey(0.0027, 15)
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("alpha/2", "alpha", "1-alpha", "1-alpha/2"))
})

test_that("c4 works with vector input", {
  result <- c4(2:5)
  expect_length(result, 4)
  expect_equal(result[1], c4(2))
  expect_equal(result[2], c4(3))
  expect_equal(result[3], c4(4))
  expect_equal(result[4], c4(5))
})

test_that("alpha.risk with n=2 returns correct value", {
  result <- alpha.risk(2)
  expect_true(is.numeric(result))
  expect_true(result > 0)
  expect_true(result > 0.0027)
})

test_that("table.const with n=2 returns single row", {
  result <- table.const(2)
  expect_equal(nrow(result), 1)
  expect_equal(rownames(result), "2")
})

test_that("d3 with n=2 returns correct value", {
  expect_equal(d3(2), 0.8525025, tolerance = 1e-5)
})

test_that("d2 reference values match Montgomery Table VI", {
  expect_equal(d2(2), 1.128, tolerance = 1e-3)
  expect_equal(d2(3), 1.693, tolerance = 1e-3)
  expect_equal(d2(4), 2.059, tolerance = 1e-3)
  expect_equal(d2(5), 2.326, tolerance = 1e-3)
  expect_equal(d2(6), 2.534, tolerance = 1e-3)
  expect_equal(d2(7), 2.704, tolerance = 1e-3)
  expect_equal(d2(8), 2.847, tolerance = 1e-3)
})

test_that("d3 reference values match Montgomery Table VI", {
  expect_equal(d3(2), 0.853, tolerance = 1e-3)
  expect_equal(d3(3), 0.888, tolerance = 1e-3)
  expect_equal(d3(4), 0.880, tolerance = 1e-3)
  expect_equal(d3(5), 0.864, tolerance = 1e-3)
})

test_that("c4 reference values match Montgomery Table VI", {
  expect_equal(c4(2), 0.798, tolerance = 1e-3)
  expect_equal(c4(3), 0.886, tolerance = 1e-3)
  expect_equal(c4(4), 0.921, tolerance = 1e-3)
  expect_equal(c4(5), 0.940, tolerance = 1e-3)
})
