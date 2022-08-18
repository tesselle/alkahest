## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Linear baseline
BEGe_linear <- baseline_linear(x = BEGe, from = 8, to = 20)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_linear, type = "l", col = "red")

## Rubberband baseline
BEGe_rubber <- baseline_rubberband(BEGe)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_rubber, type = "l", col = "red")

## SNIP baseline
BEGe_snip <- baseline_snip(BEGe, LLS = FALSE, decreasing = FALSE, n = 100)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_snip, type = "l", col = "red")
