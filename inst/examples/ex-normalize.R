## Raman spectrometry
data("Raman")

## Subset from 200 to 800 1/cm
Raman <- signal_select(Raman, from = 200, to = 800)

## Plot spectrum
plot(Raman, type = "l", xlab = "Raman shift", ylab = "Intensity")

## Normalize SNV
Raman_snv <- rescale_snv(Raman)
plot(Raman_snv, type = "l", xlab = "Raman shift", ylab = "Intensity")
