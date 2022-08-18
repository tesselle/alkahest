## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rescale so that intensities sum to 1
BEGe_total <- rescale_total(BEGe, total = 1)
plot(BEGe_total, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rescale intensities to 0-1
BEGe_range <- rescale_range(BEGe, min = 0, max = 1)
plot(BEGe_range, type = "l", xlab = "Energy (keV)", ylab = "Count")
