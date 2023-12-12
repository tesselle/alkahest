# RESAMPLE
#' @include AllGenerics.R
NULL

# Bin ==========================================================================
#' @export
#' @rdname resample_bin
#' @aliases resample_bin,numeric,numeric-method
setMethod(
  f = "resample_bin",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, by, f = mean, ...) {
    assert_length(y, length(x))

    k <- length(x) %% by
    if (k != 0) {
      x <- utils::tail(x, -k)
      y <- utils::tail(y, -k)
    }
    i <- cut(seq_along(x), breaks = length(x) / by, labels = FALSE)

    mid <- tapply(X = x, INDEX = i, FUN = mean, simplify = FALSE)
    bin <- tapply(X = y, INDEX = i, FUN = f, ..., simplify = FALSE)

    xy <- list(x = unlist(mid), y = unlist(bin))
    xy
  }
)

#' @export
#' @rdname resample_bin
#' @aliases resample_bin,ANY,missing-method
setMethod(
  f = "resample_bin",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, y, by, f = sum) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, by = by, f = f)
  }
)

# Downsample ===================================================================
#' @export
#' @rdname resample_down
#' @aliases resample_down,numeric,numeric-method
setMethod(
  f = "resample_down",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, by) {
    assert_length(y, length(x))

    i <- seq(from = 1, to = length(x), by = by)
    xy <- list(x = x[i], y = y[i])
    xy
  }
)

#' @export
#' @rdname resample_down
#' @aliases resample_down,ANY,missing-method
setMethod(
  f = "resample_down",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, y, by) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, by = by)
  }
)

# Interpolate ==================================================================
#' @export
#' @rdname resample_interpolate
#' @aliases resample_interpolate,numeric,numeric-method
setMethod(
  f = "resample_interpolate",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, from, to, by, ...) {
    assert_length(y, length(x))

    ## New x scale
    x_scale <- seq(from = from, to = to, by = by)

    ## Interpolate
    new_data <- stats::approx(x = x, y = y, xout = x_scale, ...)

    xy <- list(x = new_data$x, y = new_data$y)
    xy
  }
)

#' @export
#' @rdname resample_interpolate
#' @aliases resample_interpolate,ANY,missing-method
setMethod(
  f = "resample_interpolate",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, y, from, to, by, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, from = from, to = to, by = by, ...)
  }
)
