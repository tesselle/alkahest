## X-ray diffraction
data("XRD")

## Subset from 20 to 70 degrees
XRD <- signal_select(XRD, from = 20, to = 70)

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Polynomial baseline
baseline <- baseline_polynomial(XRD, d = 4, tolerance = 0.02, stop = 1000)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")
