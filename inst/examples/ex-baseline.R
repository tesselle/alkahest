## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rubberband baseline
BEGe_rubber <- baseline_rubberband(BEGe)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_rubber, type = "l", col = "red")

## SNIP baseline
BEGe_snip <- baseline_snip(BEGe, LLS = FALSE, decreasing = FALSE, n = 100)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_snip, type = "l", col = "red")

## 4S Peak Filling
BEGe_peak <- baseline_peakfilling(BEGe, n = 5, m = 5, by = 2)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_peak, type = "l", col = "red")

