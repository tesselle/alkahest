## X-ray diffraction
data("XRD")

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Linear baseline
baseline <- baseline_linear(XRD, points = c(25, 34))

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")

## Correct baseline
XRD$count <- XRD$count - baseline$y

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
