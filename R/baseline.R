# BASELINE
#' @include AllGenerics.R
NULL

# Linear =======================================================================
#' @export
#' @rdname baseline_linear
#' @aliases baseline_linear,numeric,numeric-method
setMethod(
  f = "baseline_linear",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, points = range(x)) {
    assert_length(y, length(x))

    ## Find the nearest value
    z <- vapply(X = points, FUN = function(i, x) which_nearest(x, i),
                FUN.VALUE = numeric(1), x = x)
    zi <- which(x >= min(points) & x <= max(points))

    data <- data.frame(x, y)
    fit <- stats::lm(y ~ x, data = data, subset = z)
    pred <- stats::predict(fit, data.frame(x = x[zi]))

    bsl <- rep(NA_real_, length(x))
    bsl[zi] <- pred

    xy <- list(x = x, y = bsl)
    attr(xy, "method") <- "linear baseline"
    xy
  }
)

#' @export
#' @rdname baseline_linear
#' @aliases baseline_linear,ANY,missing-method
setMethod(
  f = "baseline_linear",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, points = range(x)) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, points = points)
  }
)

# Polynomial ===================================================================
#' @export
#' @rdname baseline_polynomial
#' @aliases baseline_polynomial,numeric,numeric-method
setMethod(
  f = "baseline_polynomial",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, d = 3, tolerance = 0.001, stop = 100) {
    assert_length(y, length(x))

    polynom <- cbind(1 / sqrt(length(x)), stats::poly(x, degree = d))

    old_y <- y
    start <- 0
    convergence <- FALSE
    while (!convergence) {
      new_y <- polynom %*% crossprod(polynom, old_y)
      new_y <- pmin(y, new_y)

      criterion <- sum(abs((new_y - old_y) / old_y), na.rm = TRUE)
      convergence <- criterion < tolerance

      old_y <- new_y
      start <- start + 1
      if (start >= stop) {
        warning("Convergence not reached.", call. = FALSE)
        break
      }
    }

    xy <- list(x = x, y = new_y)
    attr(xy, "method") <- "polynomial baseline"
    xy
  }
)

#' @export
#' @rdname baseline_polynomial
#' @aliases baseline_polynomial,ANY,missing-method
setMethod(
  f = "baseline_polynomial",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, d = 3, tolerance = 0.001, stop = 100) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, d = d, tolerance = tolerance,
                         stop = stop)
  }
)

# Rolling Ball =================================================================
#' @export
#' @rdname baseline_rollingball
#' @aliases baseline_rollingball,numeric,numeric-method
setMethod(
  f = "baseline_rollingball",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, m, s) {
    n <- length(x)
    assert_length(y, n)

    ## Windows
    win_minmax <- window_sliding(n, m)
    win_smooth <- window_sliding(n, s)

    ## Minimize
    T1 <- vapply(
      X = win_minmax,
      FUN = function(i, data) {
        min(data[i])
      },
      FUN.VALUE = numeric(1),
      data = y
    )

    ## Maximize
    T2 <- vapply(
      X = win_minmax,
      FUN = function(i, data) {
        max(data[i])
      },
      FUN.VALUE = numeric(1),
      data = T1
    )

    ## Smooth
    T3 <- vapply(
      X = win_smooth,
      FUN = function(i, data) {
        mean(data[i])
      },
      FUN.VALUE = numeric(1),
      data = T2
    )

    xy <- list(x = x, y = T3)
    attr(xy, "method") <- "rolling ball baseline"
    xy
  }
)

#' @export
#' @rdname baseline_rollingball
#' @aliases baseline_rollingball,ANY,missing-method
setMethod(
  f = "baseline_rollingball",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, m, s) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, m = m, s = s)
  }
)

# Rubberband ===================================================================
#' @export
#' @rdname baseline_rubberband
#' @aliases baseline_rubberband,numeric,numeric-method
setMethod(
  f = "baseline_rubberband",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, noise = 0, spline = TRUE, ...) {
    assert_length(y, length(x))

    ## (chull returns points in clockwise order)
    pts <- grDevices::chull(x, y)

    ## Check that ncol(y) is a position 1,
    ## if not, rotate chull vertex index so that ncol(y) is at position 1
    ## then keep only index from ncol(y) to 1 (i.e. lower part of the hull)
    v_max <- which.max(pts) - 1
    if (v_max > 0) pts <- c(pts[-seq_len(v_max)], pts[seq_len(v_max)])
    pts <- pts[1:which.min(pts)]
    # First and last point must be minima, if not remove them
    if (pts[2] == pts[1] - 1) pts <- pts[-1] # Last point
    pts <- rev(pts) # Sort in ascending order
    if (pts[2] == pts[1] + 1) pts <- pts[-1] # First point

    bsl <- stats::approx(x = x[pts], y = y[pts], xout = x, method = "linear")$y
    if (spline) {
      pts <- which(y <= bsl + noise)
      if (length(pts) > 3) {
        spl <- stats::smooth.spline(x[pts], y[pts], ...)
        tmp <- stats::predict(spl, x, 0)$y
      } else {
        tmp <- stats::spline(x[pts], y[pts], xout = x)$y
      }
    }

    ## Check baseline
    if (anyNA(tmp))
      stop("Failed to estimate the baseline, please check your parameters.",
           call. = FALSE)

    xy <- list(x = x, y = bsl)
    attr(xy, "method") <- "rubberband baseline"
    xy
  }
)

#' @export
#' @rdname baseline_rubberband
#' @aliases baseline_rubberband,ANY,missing-method
setMethod(
  f = "baseline_rubberband",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, noise = 0, spline = TRUE, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, noise = noise, spline = spline, ...)
  }
)

# SNIP =========================================================================
#' @export
#' @rdname baseline_snip
#' @aliases baseline_snip,numeric,numeric-method
setMethod(
  f = "baseline_snip",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, LLS = FALSE, decreasing = FALSE, n = 100) {
    assert_length(y, length(x))

    ## LLS operator
    y <- if (LLS) LLS(y) else y

    k <- length(y)
    iter <- if (decreasing) rev(seq_len(n)) else seq_len(n)

    tmp <- y
    for (p in iter) {
      for (i in p:(k - p)) {
        a <- y[i]
        b <- (y[i - p] + y[i + p]) / 2
        tmp[i] <- min(a, b)
      }
      y <- tmp
    }

    ## Inverse LLS operator
    bsl <- if (LLS) inverseLLS(y) else y

    ## Check baseline
    if (anyNA(bsl))
      stop("Failed to estimate the baseline, please check your parameters.",
           call. = FALSE)

    xy <- list(x = x, y = bsl)
    attr(xy, "method") <- "SNIP"
    xy
  }
)

#' @export
#' @rdname baseline_snip
#' @aliases baseline_snip,ANY,missing-method
setMethod(
  f = "baseline_snip",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, LLS = FALSE, decreasing = FALSE, n = 100) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, LLS = LLS, decreasing = decreasing,
                         n = n)
  }
)

#' LLS operator
#'
#' @param x A [`numeric`] vector.
#' @keywords internal
#' @noRd
LLS <- function(x) {
  log(log(sqrt(x + 1) + 1) + 1)
}
inverseLLS <- function(x) {
  (exp(exp(x) - 1) - 1)^2 - 1
}

# AsLS =========================================================================
#' @export
#' @rdname baseline_asls
#' @aliases baseline_asls,numeric,numeric-method
setMethod(
  f = "baseline_asls",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, p = 0.01, lambda = 10^4, stop = 100) {
    assert_Matrix()
    assert_length(y, length(x))

    m <- length(y)
    E <- Matrix::Diagonal(m)
    D <- Matrix::diff(E, lag = 1, differences = 2)

    w <- rep(1, m) # weights

    start <- 0
    convergence <- FALSE
    while (!convergence) {
      W <- Matrix::Diagonal(x = w)
      C <- Matrix::chol(W + (lambda * Matrix::t(D) %*% D))
      z <- Matrix::solve(C, Matrix::solve(Matrix::t(C), w * y))
      ## Prior to v1.6, Matrix::solve(a=<Matrix>, b=<vector>) returns a matrix
      z <- as.numeric(z)
      w0 <- p * (y > z) + (1 - p) * (y < z)
      if (isTRUE(all.equal(w, w0))) convergence <- TRUE

      w <- w0
      start <- start + 1
      if (start >= stop) {
        warning("Convergence not reached.", call. = FALSE)
        break
      }
    }

    xy <- list(x = x, y = z)
    attr(xy, "method") <- "AsLS"
    xy
  }
)

#' @export
#' @rdname baseline_asls
#' @aliases baseline_asls,ANY,missing-method
setMethod(
  f = "baseline_asls",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, p = 0.01, lambda = 10^4, stop = 100) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lambda = lambda, p = p, stop = stop)
  }
)

# Peak Filling =================================================================
#' @export
#' @rdname baseline_peakfilling
#' @aliases baseline_peakfilling,numeric,numeric-method
setMethod(
  f = "baseline_peakfilling",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, n, m, by = 10, lambda = 1600, d = 2, sparse = FALSE) {
    assert_length(y, length(x))

    ## Number of bucket intervals
    by <- length(x) / by

    ## Exponential decrease in interval width
    win <- m
    if (n != 1) {
      i <- log10(m) * (1 - (0:(n - 2)) / (n - 1))
      win <- ceiling((10)^c(i, 0))
    }

    ## Smoothing
    smoothed <- smooth_whittaker(x, y, lambda = lambda, d = d, sparse = sparse)
    x <- smoothed$x
    y <- smoothed$y

    ## Subsampling
    breaks <- cut(seq_along(x), breaks = by, labels = FALSE)
    mid <- unlist(tapply(X = x, INDEX = breaks, FUN = mean, simplify = FALSE))
    bin <- unlist(tapply(X = y, INDEX = breaks, FUN = min, simplify = FALSE))

    ## Suppression
    iter <- seq_len(n)
    for (i in iter) {
      win_0 <- win[i] # Current half window width

      ## Interval cut-off close to edges
      k <- seq(from = 2, to = by - 1, by = 1)
      j <- by - k + 1
      v <- pmin(k - 1, win_0, by - k)

      ## Point-wise iteration to the right
      a <- mapply(function(k, v, bin) { mean(bin[(k - v):(k + v)]) }, k, v,
                  MoreArgs = list(bin = bin))
      bin[k] <- pmin(a, bin[k]) # Baseline suppression

      ## Point-wise iteration to the left
      a <- mapply(function(j, v, bin) { mean(bin[(j - v):(j + v)]) }, j, v,
                  MoreArgs = list(bin = bin))
      bin[j] <- pmin(a, bin[j]) # Baseline suppression
    }

    ## Stretch
    mid[1] <- x[1]
    mid[by] <- x[length(x)]
    bsl <- stats::approx(mid, bin, x)$y

    xy <- list(x = x, y = bsl)
    attr(xy, "method") <- "4S Peak Filling"
    xy
  }
)

#' @export
#' @rdname baseline_peakfilling
#' @aliases baseline_peakfilling,ANY,missing-method
setMethod(
  f = "baseline_peakfilling",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, n, m, by = 10, lambda = 1600, d = 2, sparse = FALSE) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, n = n, m = m, by = by,
                         lambda = lambda, d = d, sparse = sparse)
  }
)
