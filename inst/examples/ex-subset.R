## gamma-ray spectrometry
data("BEGe")

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Subset from 2.75 keV to 200 keV
BEGe_1 <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe_1, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Subset from the 20th to the 1250th value
BEGe_2 <- signal_slice(BEGe, subset = 20:1250)

## Plot spectrum
plot(BEGe_2, type = "l", xlab = "Energy (keV)", ylab = "Count")
