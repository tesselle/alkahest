## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Correct baseline
BEGe_correct <- signal_correct(BEGe, method = "SNIP")

## Plot spectrum
plot(BEGe_correct, type = "l", xlab = "Energy (keV)", ylab = "Count")
abline(h = mad(BEGe_correct$y) * 3) # noise threshold

## Find peaks
BEGe_peaks <- peaks_find(BEGe_correct, SNR = 3, m = 5)
lines(BEGe_peaks, type = "p", pch = 16, col = "red")

## gamma-ray spectrometry
data("LaBr")

## Plot spectrum
LaBr <- signal_select(LaBr, from = 1350, to = 1650)
plot(LaBr, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Half-Width at Half-Maximum
peaks_fwhm(LaBr, center = 1490)
