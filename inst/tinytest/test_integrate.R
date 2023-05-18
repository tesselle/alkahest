# Rectangle rule ===============================================================
x <- seq(0, 2, len = 101)
y <- x^3

expect_equal(integrate_rectangle(x, y, right = FALSE), 3.9204)
expect_equal(integrate_rectangle(x, y, right = TRUE), 4.0804)

# Trapezoidal rule =============================================================
x <- seq(0, 2, len = 101)
y <- x^3

expect_equal(integrate_trapezoid(x, y), 4.0004)
