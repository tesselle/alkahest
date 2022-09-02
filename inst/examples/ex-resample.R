## X-ray diffraction
data("XRD")

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Bin by 3
XRD_bin <- resample_bin(XRD, by = 3)
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(XRD_bin, type = "l", col = "red")

## Downsample by 10
XRD_down <- resample_down(XRD, by = 10)
plot(XRD, type = "l", xlim = c(20, 40), xlab = expression(2*theta), ylab = "Count")
lines(XRD_down, type = "l", col = "red")

## Linearly interpolate
XRD_approx <- resample_interpolate(XRD, from = 20, to = 40, by = 0.02)
plot(XRD, type = "l", xlim = c(20, 40), xlab = expression(2*theta), ylab = "Count")
lines(XRD_approx, type = "l", col = "red")
