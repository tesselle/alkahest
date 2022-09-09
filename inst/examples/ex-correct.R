## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Drift
baseline <- baseline_snip(BEGe)
BEGe_drif <- signal_drift(BEGe, lag = baseline, subtract = TRUE)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_drif, type = "l", col = "red")

## Correct
BEGe_corr <- signal_correct(BEGe, method = "SNIP")

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_corr, type = "l", col = "red")
