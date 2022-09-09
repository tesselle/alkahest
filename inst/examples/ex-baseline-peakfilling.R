## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## 4S Peak Filling
baseline <- baseline_peakfilling(BEGe, n = 5, m = 5, by = 2)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(baseline, type = "l", col = "red")
