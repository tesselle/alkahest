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

## Baseline correction
BEGe_corr <- signal_correct(BEGe, method = "SNIP")

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_corr, type = "l", col = "red")
