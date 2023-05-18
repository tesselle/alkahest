data("XRD")

# Replace values below an arbitrary threshold ==================================
XRD_threshold <- replace_threshold(XRD, threshold = 4000, value = 5000)
expect_true(all(XRD_threshold$y > 4000))

# Replace values below a computed threshold ====================================
XRD_threshold <- replace_threshold(XRD, threshold = mean, value = 5000)
expect_true(all(XRD_threshold$y > mean(XRD$count)))

# Replace negative values ======================================================
XRD_rescale <- rescale_min(XRD, min = -1000)
expect_true(any(XRD_rescale$y < 0))

XRD_negative <- replace_negative(XRD_rescale, value = 0)
expect_false(any(XRD_negative$y < 0))
