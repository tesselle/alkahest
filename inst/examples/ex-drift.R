## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Drift
BEGe_plus <- signal_drift(BEGe, lag = 250)
BEGe_minus <- signal_drift(BEGe, lag = 250, subtract = TRUE)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_plus, type = "l", col = "red")
lines(BEGe_minus, type = "l", col = "green")
