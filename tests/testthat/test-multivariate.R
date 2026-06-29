mu <- c(5.682, 88.22)
Sigma <- miscTools::symMatrix(c(3.770, -5.495, 13.53), 2)

test_that("data.1 generates matrix for individual observations", {
  set.seed(42)
  result <- data.1(50, 1, mu, Sigma)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 50)
  expect_equal(ncol(result), 2)
})

test_that("data.1 generates array for subgroup observations", {
  set.seed(42)
  result <- data.1(20, 10, mu, Sigma)
  expect_true(is.array(result))
  expect_equal(dim(result), c(10, 2, 20))
})

test_that("stats returns list of 3 elements for individual observations", {
  set.seed(42)
  datum <- data.1(50, 1, mu, Sigma)
  result <- stats(datum, 50, 1, 2)
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("stats returns list of 3 elements for subgroup observations", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  result <- stats(datum, 20, 10, 2)
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("T2.1 returns vector of length m for individual observations", {
  set.seed(42)
  datum <- data.1(50, 1, mu, Sigma)
  estat <- stats(datum, 50, 1, 2)
  result <- T2.1(estat, 50, 1)
  expect_true(is.vector(result))
  expect_length(result, 50)
  expect_true(all(result >= 0))
})

test_that("T2.1 returns vector of length m for subgroup observations", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  result <- T2.1(estat, 20, 10)
  expect_true(is.vector(result))
  expect_length(result, 20)
  expect_true(all(result >= 0))
})

test_that("T2.2 returns single T2 value for individual observations", {
  set.seed(42)
  datum <- data.1(50, 1, mu, Sigma)
  estat <- stats(datum, 50, 1, 2)
  datum2 <- data.2(estat, 1, p = 2)
  result <- T2.2(datum2, estat, 1)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 1)
  expect_true(result >= 0)
})

test_that("T2.2 returns single T2 value for subgroup observations", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  datum2 <- data.2(estat, 10, p = 2)
  result <- T2.2(datum2, estat, 10)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 1)
  expect_true(result >= 0)
})

test_that("cchart.T2.1 runs for individual observations", {
  set.seed(42)
  datum <- data.1(50, 1, mu, Sigma)
  estat <- stats(datum, 50, 1, 2)
  T2 <- T2.1(estat, 50, 1)
  expect_no_error(cchart.T2.1(T2, 50, 1, 2))
})

test_that("cchart.T2.1 runs for subgroup observations", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  T2 <- T2.1(estat, 20, 10)
  expect_no_error(cchart.T2.1(T2, 20, 10, 2))
})

test_that("cchart.T2.1 errors on n < 1", {
  set.seed(42)
  datum <- data.1(50, 1, mu, Sigma)
  estat <- stats(datum, 50, 1, 2)
  T2 <- T2.1(estat, 50, 1)
  expect_error(cchart.T2.1(T2, 50, 0, 2), "n must be equal to or higher than 1")
})

test_that("cchart.T2.2 runs without error", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  datum2 <- data.2(estat, 10, p = 2)
  T2II <- T2.2(datum2, estat, 10)
  expect_no_error(cchart.T2.2(T2II, 20, 10, 1, 25, 2))
})

test_that("cchart.T2.2 with phase I data runs without error", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  datum2 <- data.2(estat, 10, p = 2)
  T2II <- T2.2(datum2, estat, 10)
  expect_no_error(cchart.T2.2(T2II, 20, 10, 1, 25, 2, datum = datum))
})

test_that("remove.data removes observation from matrix", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  result <- remove.data(datum, 13)
  expect_equal(dim(result)[3], 19)
})

test_that("remove.data removes observation from 2D matrix", {
  mat <- matrix(1:20, nrow = 10, ncol = 2)
  result <- remove.data(mat, 5)
  expect_equal(nrow(result), 9)
})

test_that("data.2 generates data with delta shift", {
  set.seed(42)
  datum <- data.1(20, 10, mu, Sigma)
  estat <- stats(datum, 20, 10, 2)
  result <- data.2(estat, 10, delta = 1, p = 2)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 2)
})
