# Odd integer ==================================================================
x <- 2
expect_error(alkahest:::assert_odd(x))

x <- 3
expect_equal(alkahest:::assert_odd(x), x)
