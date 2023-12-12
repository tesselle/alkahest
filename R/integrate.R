# SIGNAL INTEGRATION
#' @include AllGenerics.R
NULL

#' @export
#' @rdname integrate_rectangle
#' @aliases integrate_rectangle,numeric,numeric-method
setMethod(
  f = "integrate_rectangle",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, right = FALSE) {
    assert_length(y, length(x))

    h <- if (right) utils::tail(y, -1) else utils::head(y, -1)
    sum(h * diff(x))
  }
)

#' @export
#' @rdname integrate_rectangle
#' @aliases integrate_rectangle,ANY,missing-method
setMethod(
  f = "integrate_rectangle",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, right = FALSE) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, right = right)
  }
)

#' @export
#' @rdname integrate_trapezoid
#' @aliases integrate_trapezoid,numeric,numeric-method
setMethod(
  f = "integrate_trapezoid",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    assert_length(y, length(x))

    sum((utils::head(y, -1) + utils::tail(y, -1)) * diff(x) / 2)
  }
)

#' @export
#' @rdname integrate_trapezoid
#' @aliases integrate_trapezoid,ANY,missing-method
setMethod(
  f = "integrate_trapezoid",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y)
  }
)
