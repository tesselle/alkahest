## X-ray diffraction
data("XRD")

XRD1 <- signal_drift(XRD, lag = 1500)

## Bind
XRD_bind <- signal_bind(XRD, XRD1)
XRD_bind[, 1:10]

## Mean
XRD_mean <- signal_mean(XRD, XRD1)

plot(NULL, type = "l", xlim = c(10, 70) , ylim = c(3000, 36000),
     xlab = expression(2*theta), ylab = "Count")
lines(XRD, type = "l")
lines(XRD1, type = "l")
lines(XRD_mean, type = "l", col = "red")
