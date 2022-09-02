## X-ray diffraction
data("XRD")

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Shift by one degree
XRD_offset <- signal_shift(XRD, lag = 1)
lines(XRD_offset, type = "l", col = "red")
