test_that("Offset", {
  data("XRD")

  ## Shift by one degree
  offset <- shift_offset(XRD, delta = 1)
  expect_equal(offset$x, XRD$theta + 1)

  # plot(XRD, type = "l", xlab = "", ylab = "")
  # lines(offset, type = "l", col = "red")
})
test_that("Interpolate", {
  data("XRD")

  ## Linearly interpolate
  interpolate <- shift_interpolate(XRD, from = 20, to = 40, by = 0.1)
  expect_snapshot(interpolate)

  # plot(XRD, type = "l", xlim = c(20, 40), xlab = "", ylab = "")
  # lines(interpolate, type = "l", col = "red")
})
