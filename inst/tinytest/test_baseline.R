if (at_home()) {
  data("XRD")

  # Linear baseline ============================================================
  linear <- baseline_linear(XRD, points = c(10, 70))

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(linear, type = "l", col = "red")

  expect_equal_to_reference(linear, file = "_snaps/baseline_linear.rds")

  # Polynomial baseline ========================================================
  XRD2 <- signal_select(XRD, from = 20, to = 70)
  polynomial <- baseline_polynomial(XRD2, d = 4, tolerance = 0.02, stop = 1000)

  # plot(XRD2, type = "l", xlab = "", ylab = "")
  # lines(polynomial, type = "l", col = "red")

  expect_equal_to_reference(polynomial, file = "_snaps/baseline_polynomial.rds")

  # Rolling Ball baseline ======================================================
  rollingball <- baseline_rollingball(XRD, m = 201, s = 151)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(rollingball, type = "l", col = "red")

  expect_equal_to_reference(rollingball, file = "_snaps/baseline_rollingball.rds")

  # Rubberband baseline ========================================================
  rubberband <- baseline_rubberband(XRD)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(rubberband, type = "l", col = "red")

  expect_equal_to_reference(rubberband, file = "_snaps/baseline_rubberband.rds")

  # SNIP baseline ==============================================================
  snip <- baseline_snip(XRD, LLS = FALSE, decreasing = FALSE)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip, type = "l", col = "red")

  expect_equal_to_reference(snip, file = "_snaps/baseline_snip.rds")

  snip_LLS <- baseline_snip(XRD, LLS = TRUE, decreasing = FALSE, n = 50)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip_LLS, type = "l", col = "red")

  expect_equal_to_reference(snip_LLS, file = "_snaps/baseline_snip_LLS.rds")

  snip_decreasing <- baseline_snip(XRD, LLS = TRUE, decreasing = TRUE, n = 50)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(snip_decreasing, type = "l", col = "red")

  expect_equal_to_reference(snip_decreasing, file = "_snaps/baseline_snip_decreasing.rds")

  # 4S Peak Filling ============================================================
  peak <- baseline_peakfilling(XRD, n = 10, m = 5, by = 10, sparse = TRUE)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(peak, type = "l", col = "red")

  expect_equal_to_reference(peak, file = "_snaps/baseline_peakfilling.rds")
}
