## gamma-ray spectrometry
data("LaBr")

## Plot spectrum
LaBr <- signal_select(LaBr, from = 1350, to = 1650)
plot(LaBr, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rectangular smooth
LaBr_unweighted <- smooth_rectangular(LaBr, m = 3)
plot(LaBr_unweighted, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Triangular smooth
LaBr_weighted <- smooth_triangular(LaBr, m = 5)
plot(LaBr_weighted, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Savitzky–Golay
LaBr_savitzky <- smooth_savitzky(LaBr, m = 21, p = 2)
plot(LaBr_savitzky, type = "l", xlab = "Energy (keV)", ylab = "Count")
