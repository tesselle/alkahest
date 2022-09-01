## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Transform intensities
BEGe_trans <- rescale_transform(BEGe, f = sqrt)
plot(BEGe_trans, type = "l", xlab = "Energy (keV)", ylab = "sqrt(Count)")
