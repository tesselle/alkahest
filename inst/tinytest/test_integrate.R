x <- seq(0, 2, len = 101)
y <- x^3
z <- data.frame(x, y)

# Rectangle rule ===============================================================
expect_equal(integrate_rectangle(z, right = FALSE), 3.9204)
expect_equal(integrate_rectangle(z, right = TRUE), 4.0804)

# Trapezoidal rule =============================================================
expect_equal(integrate_trapezoid(z), 4.0004)
