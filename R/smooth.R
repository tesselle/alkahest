# SMOOTHING
#' @include AllGenerics.R
NULL

# Unweighted smoothing =========================================================
#' @export
#' @rdname smooth_rectangular
#' @aliases smooth_rectangular,numeric,numeric-method
setMethod(
  f = "smooth_rectangular",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3) {
    assert_length(y, length(x))

    ## Windows
    index <- window_sliding(length(x), m)

    y <- vapply(
      X = index,
      FUN = function(i, data) mean(data[i]),
      FUN.VALUE = numeric(1),
      data = y
    )

    xy <- list(x = x, y = y)
    attr(xy, "method") <- "unweighted smoothing"
    xy
  }
)

#' @export
#' @rdname smooth_rectangular
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
#' @rdname smooth_triangular
#' @aliases smooth_triangular,numeric,numeric-method
setMethod(
  f = "smooth_triangular",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3) {
    # Validation
    assert_length(y, length(x))
    assert_odd(m)

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
#' @rdname smooth_triangular
#' @aliases smooth_triangular,ANY,missing-method
setMethod(
  f = "smooth_triangular",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, m) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, m = m)
  }
)

# Loess smoothing ==============================================================
#' @export
#' @rdname smooth_loess
#' @aliases smooth_loess,numeric,numeric-method
setMethod(
  f = "smooth_loess",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, span = 0.75, ...) {
    assert_length(y, length(x))

    points <- data.frame(x, y)
    fit <- stats::loess(y ~ x, data = points, span = span, ...)
    bsl <- stats::predict(fit)

    xy <- list(x = x, y = bsl)
    attr(xy, "method") <- "loess smoothing"
    xy
  }
)

#' @export
#' @rdname smooth_loess
#' @aliases smooth_loess,ANY,missing-method
setMethod(
  f = "smooth_loess",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, span = 0.75, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, span = span, ...)
  }
)

# Savitzky-Golay filter ========================================================
#' @export
#' @rdname smooth_savitzky
#' @aliases smooth_savitzky,numeric,numeric-method
setMethod(
  f = "smooth_savitzky",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, m = 3, p = 2) {
    ## Validation
    assert_length(y, length(x))
    assert_odd(m)

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
#' @rdname smooth_savitzky
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

# Whittaker smoothing ==========================================================
#' @export
#' @rdname smooth_whittaker
#' @aliases smooth_whittaker,numeric,numeric-method
setMethod(
  f = "smooth_whittaker",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, lambda = 1600, d = 2, sparse = FALSE) {
    assert_length(y, length(x))
    m <- length(y)

    if (sparse) {
      assert_Matrix()

      E <- Matrix::Diagonal(m)
      D <- Matrix::diff(E, lag = 1, differences = d)
      B <- Matrix::chol(E + (lambda * Matrix::t(D) %*% D))
      z <- Matrix::solve(B, Matrix::solve(Matrix::t(B), y))
      ## Prior to v1.6, Matrix::solve(a=<Matrix>, b=<vector>) returns a matrix
      z <- as.numeric(z)
    } else {
      E <- diag(m)
      D <- diff(E, lag = 1, differences = d)
      B <- E + (lambda * t(D) %*% D)
      z <- solve(B, y)
    }

    xy <- list(x = x, y = z)
    attr(xy, "method") <- "Whittaker smoothing"
    xy
  }
)

#' @export
#' @rdname smooth_whittaker
#' @aliases smooth_whittaker,ANY,missing-method
setMethod(
  f = "smooth_whittaker",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, lambda = 1600, d = 2, sparse = FALSE) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lambda = lambda, d = d,
                         sparse = sparse)
  }
)

# Penalized likelihood smoothing ===============================================
#' @export
#' @rdname smooth_likelihood
#' @aliases smooth_likelihood,numeric,numeric-method
setMethod(
  f = "smooth_likelihood",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, lambda, d = 2, SE = FALSE,
                        progress = interactive()) {
    assert_Matrix()
    assert_length(y, length(x))

    m <- length(y)
    n <- length(lambda)

    aic <- rep(0, n)
    mu <- matrix(data = 0, nrow = m, ncol = n)
    se <- matrix(data = 0, nrow = m, ncol = n)
    res <- matrix(data = 0, nrow = m, ncol = n)

    if (progress) pb <- utils::txtProgressBar(min = 0, max = n, style=3)

    n_lambda <- seq_len(n)
    for (i in n_lambda) {
      like <- penalized_likelihood(y, lambda = lambda[i], d = d, SE = SE)

      aic[i] <- like$aic
      mu[, i] <- like$mu
      se[, i] <- like$se
      res[, i] <- like$res

      if (progress) utils::setTxtProgressBar(pb, i)
    }

    if (progress) close(pb)

    k <- which.min(aic)
    mu <- mu[, k]
    se <- se[, k]
    res <- res[, k]
    op_lambda <- lambda[k]
    op_aic <- aic[k]

    xy <- list(x = x, y = mu, aic = aic, se = se, residuals = res,
               optimal_aic = op_aic, optimal_lambda = op_lambda)
    attr(xy, "method") <- "penalized likelihood smoothing"
    xy
  }
)

#' @export
#' @rdname smooth_likelihood
#' @aliases smooth_likelihood,ANY,missing-method
setMethod(
  f = "smooth_likelihood",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, lambda, d = 2, SE = FALSE,
                        progress = interactive()) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lambda = lambda, d = d,
                         SE = SE, progress = progress)
  }
)

penalized_likelihood <- function(y, lambda, d = 2, SE = FALSE) {

  m <- length(y)
  z <- log(y + 1)
  E <- Matrix::Diagonal(m)
  D <- Matrix::diff(E, differences = d)
  P <- lambda * Matrix::crossprod(D, D)
  mu <- exp(z)

  delta_mu <- 1
  limit <- 1 # Stop condition to prevent infinite loop
  while (delta_mu >= 0.0001 & limit <= 10) {
    W <- Matrix::Diagonal(x = mu)
    z <- Matrix::solve(W + P, y - mu + mu * z)
    new_mu <- exp(as.numeric(z)) # Drop S4 class
    delta_mu <- max(abs(new_mu - mu))
    mu <- new_mu

    limit <- limit + 1
  }

  ## AIC
  H <- Matrix::solve(W + P) %*% W
  ed <- sum(Matrix::diag(H))
  dev <- 2 * sum(y * log((y + 1e-10) / mu))
  aic <- dev + 2 * ed * m / (m - ed)

  ## Standard error
  se <- rep(NA_real_, m)
  if (isTRUE(SE)) {
    HH <- Matrix::tcrossprod(H %*% W, H)
    se <- sqrt(Matrix::diag(HH))
  }

  ## Standardized residuals for counts
  res <- (y - mu) / sqrt(mu)

  list(mu = mu, aic = aic, se = se, res = res)
}
