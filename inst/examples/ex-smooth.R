## Simulate data with some noise
x <- seq(-4, 4, length = 100)
y <- dnorm(x) + rnorm(100, mean = 0, sd = 0.01)

## Plot spectrum
plot(x, y, type = "l", xlab = "", ylab = "")

## Rectangular smoothing
unweighted <- smooth_rectangular(x, y, m = 3)
plot(unweighted, type = "l", xlab = "", ylab = "")

## Triangular smoothing
weighted <- smooth_triangular(x, y, m = 5)
plot(weighted, type = "l", xlab = "", ylab = "")

## Loess smoothing
loess <- smooth_loess(x, y, span = 0.75)
plot(loess, type = "l", xlab = "", ylab = "")

## Savitzky–Golay filter
savitzky <- smooth_savitzky(x, y, m = 21, p = 2)
plot(savitzky, type = "l", xlab = "", ylab = "")

## Whittaker smoothing
whittaker <- smooth_whittaker(x, y, lambda = 1600, d = 2)
plot(whittaker, type = "l", xlab = "", ylab = "")
