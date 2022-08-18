# SIGNAL PROCESSING
#' @include AllGenerics.R
NULL

# Subset =======================================================================
#' @export
#' @rdname subset
#' @aliases signal_select,numeric,numeric-method
setMethod(
  f = "signal_select",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, from, to) {
    ## Find the nearest value
    from <- x[which_nearest(x, from)]
    to <- x[which_nearest(x, to)]
    idx <- which(x >= from & x <= to)

    xy <- list(x = x[idx], y = y[idx])
    xy
  }
)

#' @export
#' @rdname subset
#' @aliases signal_select,ANY,missing-method
setMethod(
  f = "signal_select",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, from, to) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, from = from, to = to)
  }
)

#' @export
#' @rdname subset
#' @aliases signal_slice,numeric,numeric-method
setMethod(
  f = "signal_slice",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, subset) {
    i <- as.integer(subset)

    if (!all(i > 0) & !all(i < 0)) {
      stop("A vector of strictly positive or negative integers is expected.",
           call. = FALSE)
    }

    xy <- list(x = x[i], y = y[i])
    xy
  }
)

#' @export
#' @rdname subset
#' @aliases signal_slice,ANY,missing-method
setMethod(
  f = "signal_slice",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, subset) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, subset = subset)
  }
)

# Transform ====================================================================
#' @export
#' @rdname transform
#' @aliases signal_transform,numeric,numeric-method
setMethod(
  f = "signal_transform",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, f, ...) {
    y <- f(y, ...)
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname transform
#' @aliases signal_transform,ANY,missing-method
setMethod(
  f = "signal_transform",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, f, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, f = f, ...)
  }
)
