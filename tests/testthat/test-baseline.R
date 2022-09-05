test_that("Linear baseline", {
  data("XRD")

  linear <- baseline_linear(XRD, from = 28.75, to = 31.75)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(linear, type = "l", col = "red")

  expect_snapshot(linear)
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
