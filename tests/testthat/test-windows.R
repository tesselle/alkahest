test_that("Sliding windows", {
  windows <- window_sliding(n = 10, m = 5, i = NULL)
  expect_snapshot(windows)
  expect_equal(window_sliding(n = 10, m = 5, i = 3), windows[3])
  expect_equal(window_sliding(n = 10, m = 5, i = 3:5), windows[3:5])
})
test_that("Tumbling windows", {
  expect_snapshot(window_tumbling(n = 10, m = 3, drop = FALSE))
  expect_snapshot(window_tumbling(n = 10, m = 3, drop = TRUE))
})
