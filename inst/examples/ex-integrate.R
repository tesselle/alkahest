## Calculate the area under the sine curve from 0 to pi
# integrate(f = function(x) x^3, lower = 0, upper = 2)
x <- seq(0, 2, len = 101)
y <- x^3

plot(x, y, type = "l")

integrate_rectangle(x, y, right = FALSE) # 3.9204
integrate_rectangle(x, y, right = TRUE) # 4.0804
integrate_trapezoid(x, y) # 4.0004
