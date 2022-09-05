# BASELINE
#' @include AllGenerics.R
NULL

# Linear =======================================================================
#' @export
#' @rdname baseline_linear
#' @aliases baseline_linear,numeric,numeric-method
setMethod(
  f = "baseline_linear",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, from = min(x), to = max(x)) {

    ## Find the nearest value
    x_from <- which_nearest(x, from)
    x_to <- which_nearest(x, to)
    from <- x[x_from]
    to <- x[x_to]
    idx <- which(x >= from & x <= to)

    points <- data.frame(x, y)
    fit <- stats::lm(y ~ x, data = points, subset = c(x_from, x_to))
    bsl <- stats::predict(fit, points[idx, 1, drop = FALSE])

    xy <- list(x = x[idx], y = bsl)
    attr(xy, "method") <- "linear baseline"
    xy
  }
)

#' @export
#' @rdname baseline_linear
#' @aliases baseline_linear,ANY,missing-method
setMethod(
  f = "baseline_linear",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, from = min(x), to = max(x)) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, from = from, to = to)
  }
)

# Rubberband ===================================================================
#' @export
#' @rdname baseline_rubberband
#' @aliases baseline_rubberband,numeric,numeric-method
setMethod(
  f = "baseline_rubberband",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, noise = 0, spline = TRUE, ...) {

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
  signature = signature(x = "ANY", y = "missing"),
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
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, LLS = FALSE, decreasing = FALSE, n = 100) {
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
  signature = signature(x = "ANY", y = "missing"),
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

# Peak Filling =================================================================
#' @export
#' @rdname baseline_peakfilling
#' @aliases baseline_peakfilling,numeric,numeric-method
setMethod(
  f = "baseline_peakfilling",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, n, m, by = 10) {
    ## Number of bucket intervals
    by <- length(x) / by

    ## Exponential decrease in interval width
    win <- m
    if (n != 1) {
      i <- log10(m) * (1 - (0:(n - 2)) / (n - 1))
      win <- ceiling((10)^c(i, 0))
    }

    ## Smoothing
    # DIY

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
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, n, m, by = 10) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, n = n, m = m, by = by)
  }
)
