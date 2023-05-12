\dontrun{
## X-ray diffraction
data("XRD")

## Subset from 20 to 40 degrees
XRD <- signal_select(XRD, from = 20, to = 40)

## Plot diffractogram
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Penalized likelihood smoothing
lambda <- seq(from = 1, to = 8, length.out = 40)
lambda <- 10^lambda

likelihood <- smooth_likelihood(XRD, lambda = lambda, d = 3)
lines(likelihood, col = "red")

## Strip ka2
ka2 <- ka2_strip_penalized(XRD, lambda = lambda, tau = 0.5, nseg = 1)
lines(ka2, col = "blue")
}
