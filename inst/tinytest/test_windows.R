# Sliding windows ==============================================================
windows <- window_sliding(n = 10, m = 5, i = NULL)
expect_equal(window_sliding(n = 10, m = 5, i = 3), windows[3])
expect_equal(window_sliding(n = 10, m = 5, i = 3:5), windows[3:5])

# Tumbling windows =============================================================
tumbling <- window_tumbling(n = 10, m = 3, drop = FALSE)
expect_equal(tumbling, list(`1` = 1:4, `2` = 5:7, `3` = 8:10))

tumbling_drop <- window_tumbling(n = 10, m = 3, drop = TRUE)
expect_equal(tumbling_drop, list(`1` = 1:3, `2` = 4:6, `3` = 7:9))
