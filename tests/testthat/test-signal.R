test_that("Bind", {
  data("XRD")

  XRD1 <- signal_drift(XRD, lag = 1500)

  expect_snapshot(signal_bind(XRD, XRD1)[, 1:20])

  XRD1 <- signal_slice(XRD1, subset = 1:100)

  expect_error(signal_mean(XRD, XRD1))
})
test_that("Mean", {
  data("XRD")

  XRD1 <- signal_drift(XRD, lag = 1500)

  expect_snapshot(signal_mean(XRD, XRD1))

  XRD1 <- signal_slice(XRD1, subset = 1:100)

  expect_error(signal_mean(XRD, XRD1))
})
test_that("Select", {
  data("BEGe")

  ## Subset from 2.75 keV to 200 keV
  BEGe_select <- signal_select(BEGe, from = 3, to = 200)

  expect_gt(min(BEGe_select$x), 2)
  expect_lt(max(BEGe_select$x), 201)
})
test_that("Slice", {
  data("BEGe")

  ## Subset from the 20th to the 1250th value (1231 values)
  BEGe_slice <- signal_slice(BEGe, subset = 20:1250)

  expect_length(BEGe_slice$x, 1231)
  expect_length(BEGe_slice$y, 1231)
})
test_that("Shift", {
  data("XRD")

  ## Shift by one degree
  offset <- signal_shift(XRD, lag = 1)
  expect_equal(offset$x, XRD$theta + 1)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(offset, type = "l", col = "red")
})
test_that("Drift", {
  data("XRD")

  ## Drift by 500
  XRD_plus <- signal_drift(XRD, lag = 250)
  expect_equal(XRD_plus$y, XRD$count + 250)

  XRD_minus <- signal_drift(XRD, lag = XRD, subtract = TRUE)
  expect_equal(XRD_minus$y, rep(0, nrow(XRD)))

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(XRD_plus, type = "l", col = "red")
})
test_that("Correct", {
  data("XRD")

  rubberband <- signal_correct(XRD, method = "rubberband")

  # plot(XRD, type = "l", ylim = c(0, 35500), xlab = "", ylab = "")
  # lines(rubberband, type = "l", col = "red")

  expect_snapshot(rubberband)
})
