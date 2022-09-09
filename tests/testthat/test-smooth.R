noise <- c(-0.002, 0, -0.014, -0.001, -0.008, -0.001, -0.021, 0.009, 0.016,
           -0.006, -0.013, -0.01, -0.017, 0, -0.012, 0.01, 0.005, 0.003,
           0.012, 0.002, -0.022, -0.004, 0.007, 0.002, -0.006, -0.014, 0.012,
           0.015, -0.01, 0.001, -0.007, 0.01, 0.002, -0.018, -0.009, 0,
           -0.009, -0.008, -0.018, -0.007, -0.013, -0.003, -0.001, -0.002,
           0.002, -0.02, 0.012, -0.008, 0.018, -0.001, 0.009, -0.012, -0.003,
           0.012, -0.001, -0.016, 0.013, -0.015, -0.01, 0.014, 0.013, -0.003,
           0.007, 0.005, 0.008, 0.011, -0.007, 0.004, -0.005, 0.006, 0.006,
           0.003, 0.003, -0.024, 0.014, 0.001, 0.01, -0.01, 0.009, -0.003,
           -0.004, 0.004, -0.006, 0.008, -0.009, 0.006, -0.017, 0.001, -0.004,
           -0.01, -0.001, 0, 0.014, 0.007, 0.009, 0.012, 0.013, -0.007,
           0.017, -0.007)

x <- seq(-4, 4, length = 100)
y <- dnorm(x) + noise

test_that("Rectangular smoothing", {
  unweighted <- smooth_rectangular(x, y, m = 3)
  expect_snapshot(unweighted)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(unweighted, type = "l", col = "red")
})
test_that("Triangular smoothing", {
  weighted <- smooth_triangular(x, y, m = 3)
  expect_snapshot(weighted)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(weighted, type = "l", col = "red")
})
test_that("Loess smoothing", {
  loess <- smooth_loess(x, y, span = 0.2)
  expect_snapshot(loess)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(loess, type = "l", col = "red")
})
test_that("Savitzky–Golay filter", {
  savitzky <- smooth_savitzky(x, y, m = 21, p = 2)
  expect_snapshot(savitzky)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(savitzky, type = "l", col = "red")
})
test_that("Whittaker smoothing", {
  whittaker <- smooth_whittaker(x, y, lambda = 1000, d = 3, sparse = FALSE)
  expect_snapshot(whittaker)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(whittaker, type = "l", col = "red")

  skip_if_not_installed("Matrix")
  whittaker_sparse <- smooth_whittaker(x, y, lambda = 1000, d = 3, sparse = TRUE)

  expect_equal(whittaker, whittaker_sparse)

  # plot(x, y, type = "l", xlab = "", ylab = "")
  # lines(whittaker_sparse, type = "l", col = "red")
})
