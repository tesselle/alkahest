## Calculate the area under the sine curve from 0 to pi
x <- seq(0, pi, len = 101)
y <- sin(x)
integrate_trapezoid(x, y) # 1.999835504
