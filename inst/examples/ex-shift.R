## X-ray diffraction
data("XRD")

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Shift by one degree
XRD_offset <- shift_offset(XRD, delta = 1)
lines(XRD_offset, type = "l", col = "red")

## Linearly interpolate a new x scale
XRD_approx <- shift_interpolate(XRD, from = 20, to = 40, by = 0.02)
plot(XRD, type = "l", xlim = c(20, 40), xlab = expression(2*theta), ylab = "Count")
lines(XRD_approx, type = "l", col = "red")
