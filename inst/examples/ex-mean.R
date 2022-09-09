## X-ray diffraction
data("XRD")

XRD1 <- XRD
XRD1$count <- XRD1$count + 1500

## Mean
XRD_mean <- signal_mean(XRD, XRD1)

plot(NULL, type = "l", xlim = c(10, 70) , ylim = c(3000, 36000),
     xlab = expression(2*theta), ylab = "Count")
lines(XRD, type = "l")
lines(XRD1, type = "l")
lines(XRD_mean, type = "l", col = "red")
