## X-ray diffraction
data("XRD")

## Whittaker smoothing
smooth <- smooth_whittaker(XRD, lambda = 1000, d = 2, sparse = TRUE)

## 4S Peak Filling baseline
baseline <- baseline_peakfilling(smooth, n = 10, m = 5, by = 10)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")

## Correct baseline
XRD <- signal_drift(XRD, lag = baseline, subtract = TRUE)

## Find peaks
peaks <- peaks_find(XRD, SNR = 3, m = 11)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(peaks, type = "p", pch = 16, col = "red")
abline(h = attr(peaks, "noise"), lty = 2) # noise threshold

## Half-Width at Half-Maximum
x <- seq(-4, 4, length = 1000)
y <- dnorm(x)

peaks_fwhm(x, y, center = 0) # Expected: 2 * sqrt(2 * log(2))
