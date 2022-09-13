## Length of the data series
n <- 10

## Progressive sliding windows
sliding <- window_sliding(n = n, m = 5)

plot(NULL, xlim = c(1, n), ylim = c(1, 10.5), xlab = "Index", ylab = "Window")
for (i in seq_along(sliding)) {
  w <- sliding[[i]]
  text(x = w, y = rep(i, length(w)), labels = w, pos = 3)
  lines(w, rep(i, length(w)), type = "l", lwd = 2)
}

## Tumbling windows
## (compare with drop = TRUE)
tumbling <- window_tumbling(n = n, m = 3, drop = FALSE)

plot(NULL, xlim = c(1, n), ylim = c(1, 5.5), xlab = "Index", ylab = "Window")
for (i in seq_along(tumbling)) {
  w <- tumbling[[i]]
  text(x = w, y = rep(i, length(w)), labels = w, pos = 3)
  lines(w, rep(i, length(w)), type = "l", lwd = 2)
}

