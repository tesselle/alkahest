## X-ray diffraction
data("XRD")

## Subset from 20 to 70 degrees
XRD <- signal_select(XRD, from = 20, to = 70)

## Plot spectrum
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Polynomial baseline
baseline <- baseline_asls(XRD, p = 0.005, lambda = 10^7)

lines(baseline, type = "l", col = "red")
