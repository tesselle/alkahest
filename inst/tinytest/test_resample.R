data("XRD")

# Bin ==========================================================================
## Resample by 10
XRD_bin <- resample_bin(XRD, by = 3)
expect_equal(lengths(XRD_bin), c(x = 996, y = 996))

# Downsample ===================================================================
## Resample by 10
XRD_down <- resample_down(XRD, by = 10)
expect_equal(lengths(XRD_down), c(x = 299, y = 299))

# Interpolate ==================================================================
if (at_home()) {
  ## Linearly interpolate
  interpolate <- resample_interpolate(XRD, from = 20, to = 40, by = 0.1)

  interpolate_summary <- summary(as.data.frame(interpolate))
  expect_equal_to_reference(interpolate_summary, file = "_snaps/resample_interpolate.rds")

  # plot(XRD, type = "l", xlim = c(20, 40), xlab = "", ylab = "")
  # lines(interpolate, type = "l", col = "red")
}
