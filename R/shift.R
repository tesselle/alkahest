# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname shift_interpolate
#' @aliases shift_interpolate,numeric,numeric-method
setMethod(
  f = "shift_interpolate",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, from, to, by, ...) {
    ## New x scale
    x_scale <- seq(from = from, to = to, by = by)

    ## Interpolate
    new_data <- stats::approx(x = x, y = y, xout = x_scale, ...)

    xy <- list(x = new_data$x, y = new_data$y)
    xy
  }
)

#' @export
#' @rdname shift_interpolate
#' @aliases shift_interpolate,ANY,missing-method
setMethod(
  f = "shift_interpolate",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, y, from, to, by, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, from = from, to = to, by = by, ...)
  }
)
