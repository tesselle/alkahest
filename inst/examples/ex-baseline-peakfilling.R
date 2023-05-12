## X-ray diffraction
data("XRD")

## 4S Peak Filling baseline
baseline <- baseline_peakfilling(XRD, n = 10, m = 5, by = 10, sparse = TRUE)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")
