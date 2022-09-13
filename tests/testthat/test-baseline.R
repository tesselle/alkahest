test_that("Linear baseline", {
  data("XRD")

  linear <- baseline_linear(XRD, points = c(10, 70))

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(linear, type = "l", col = "red")

  expect_snapshot(linear)
})
test_that("Polynomial baseline", {
  data("XRD")

  XRD <- signal_select(XRD, from = 20, to = 70)
  polynomial <- baseline_polynomial(XRD, d = 4, tolerance = 0.02, stop = 1000)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(polynomial, type = "l", col = "red")

  expect_snapshot(polynomial)
})
test_that("Rolling Ball baseline", {
  data("XRD")

  rollingball <- baseline_rollingball(XRD, m = 201, s = 151)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(rollingball, type = "l", col = "red")

  expect_snapshot(rollingball)
})
test_that("Rubberband baseline", {
  data("XRD")

  rubberband <- baseline_rubberband(XRD)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(rubberband, type = "l", col = "red")

  expect_snapshot(rubberband)
})
test_that("SNIP baseline", {
  data("XRD")

  snip <- baseline_snip(XRD, LLS = FALSE, decreasing = FALSE)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip, type = "l", col = "red")

  expect_snapshot(snip)

  snip_LLS <- baseline_snip(XRD, LLS = TRUE, decreasing = FALSE, n = 50)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip_LLS, type = "l", col = "red")

  expect_snapshot(snip_LLS)

  snip_decreasing <- baseline_snip(XRD, LLS = TRUE, decreasing = TRUE, n = 50)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip_decreasing, type = "l", col = "red")

  expect_snapshot(snip_decreasing)
})
test_that("4S Peak Filling", {
  data("XRD")

  peak <- baseline_peakfilling(XRD, n = 10, m = 5, by = 10)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(peak, type = "l", col = "red")

  expect_snapshot(peak)
})
