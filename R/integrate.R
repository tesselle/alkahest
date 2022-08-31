# SIGNAL INTEGRATION
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate_trapezoid
#' @aliases integrate_trapezoid,numeric,numeric-method
setMethod(
  f = "integrate_trapezoid",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    sum((head(y, -1) + tail(y, -1)) * diff(x) / 2)
  }
)
