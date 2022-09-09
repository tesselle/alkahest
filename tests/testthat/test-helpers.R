test_that("Odd integer", {
  x <- 2
  expect_error(assert_odd(x))

  x <- 3
  expect_equal(assert_odd(x), x)
})
