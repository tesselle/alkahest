## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rubberband baseline
baseline <- baseline_rubberband(BEGe)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(baseline, type = "l", col = "red")
