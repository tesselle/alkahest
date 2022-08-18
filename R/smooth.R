# SMOOTHING
#' @include AllGenerics.R
NULL

# Unweighted smoothing =========================================================
#' @export
#' @rdname smooth
#' @aliases smooth_rectangular,numeric,numeric-method
setMethod(
  f = "smooth_rectangular",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3) {
    ## Validation
    if (m %% 2 == 0)
      stop(sQuote("m"), " must be an odd integer.", call. = FALSE)

    ## Index
    k <- (m - 1) / 2
    index_k <- seq_len(k)
    index_y <- seq_along(y)
    index_m <- c(index_k, rep_len(k + 1, length(y) - 2 * k), rev(index_k)) - 1

    y <- mapply(
      FUN = function(i, k, data) {
        index <- seq(from = i - k, to = i + k, by = 1)
        mean(data[index])
      },
      i = index_y,
      k = index_m,
      MoreArgs = list(data = y)
    )

    xy <- list(x = x, y = y)
    attr(xy, "method") <- "unweighted smoothing"
    xy
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_rectangular,ANY,missing-method
setMethod(
  f = "smooth_rectangular",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, m) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, m = m)
  }
)

# Weighted smoothing ===========================================================
#' @export
#' @rdname smooth
#' @aliases smooth_triangular,numeric,numeric-method
setMethod(
  f = "smooth_triangular",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3) {
    # Validation
    if (m %% 2 == 0)
      stop(sQuote("m"), " must be an odd integer.", call. = FALSE)

    # Index
    k <- (m - 1) / 2
    index_k <- seq_len(k)
    index_y <- seq_along(y)
    index_m <- c(index_k, rep_len(k + 1, length(y) - 2 * k), rev(index_k)) - 1

    y <- mapply(
      FUN = function(i, k, data) {
        j <- seq_len(k)
        w <- c(j, k + 1, rev(j))
        index <- seq(from = i - k, to = i + k, by = 1)
        stats::weighted.mean(x = data[index], w = w)
      },
      i = index_y, k = index_m,
      MoreArgs = list(data = y)
    )

    xy <- list(x = x, y = y)
    attr(xy, "method") <- "weighted smoothing"
    xy
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_triangular,ANY,missing-method
setMethod(
  f = "smooth_triangular",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, m) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, m = m)
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_savitzky,numeric,numeric-method
setMethod(
  f = "smooth_savitzky",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3, p = 2) {
    ## Validation
    if (m %% 2 == 0)
      stop(sQuote("m"), " must be an odd integer.", call. = FALSE)

    k <- (m - 1) / 2
    i <- seq(from = -k, to = k, by = 1)
    j <- utils::head(utils::tail(seq_along(y), n = -k), n = -k)
    conv <- coef_savitzky(m, p)

    smoothed <- vapply(
      X = j,
      FUN = function(j, i, conv, data) {
        sum(conv * data[j + i])
      },
      FUN.VALUE = double(1),
      i = i,
      conv = conv,
      data = y
    )
    y[j] <- smoothed

    xy <- list(x = x, y = y)
    attr(xy, "method") <- "Savitzky-Golay filter"
    xy
  }
)

#' @export
#' @rdname smooth
#' @aliases smooth_savitzky,ANY,missing-method
setMethod(
  f = "smooth_savitzky",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, m, p) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, m = m, p = p)
  }
)

coef_savitzky <- function(m, p = 2) {
  k <- (m - 1) / 2
  z <- seq(from = -k, to = k, by = 1)
  J <- vapply(X = c(0, p), FUN = function(p, z) z^p, z, FUN.VALUE = double(m))
  (solve(t(J) %*% J) %*% t(J))[1, , drop = TRUE]
}
