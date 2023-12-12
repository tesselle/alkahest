# REPLACE
#' @include AllGenerics.R
NULL

# Threshold ====================================================================
#' @export
#' @rdname replace_threshold
#' @aliases replace_threshold,numeric,numeric,function-method
setMethod(
  f = "replace_threshold",
  signature = signature(x = "numeric", y = "numeric", threshold = "function"),
  definition = function(x, y, threshold, value = 0, ...) {
    assert_length(y, length(x))

    threshold <- threshold(y, ...)
    methods::callGeneric(x, y, threshold = threshold, value = value)
  }
)

#' @export
#' @rdname replace_threshold
#' @aliases replace_threshold,ANY,missing,function-method
setMethod(
  f = "replace_threshold",
  signature = signature(x = "ANY", y = "missing", threshold = "function"),
  definition = function(x, threshold, value = 0, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, threshold = threshold,
                         value = value)
  }
)

#' @export
#' @rdname replace_threshold
#' @aliases replace_threshold,numeric,numeric,numeric-method
setMethod(
  f = "replace_threshold",
  signature = signature(x = "numeric", y = "numeric", threshold = "numeric"),
  definition = function(x, y, threshold, value = 0, ...) {
    assert_length(y, length(x))

    i <- y < threshold
    y[i] <- value

    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname replace_threshold
#' @aliases replace_threshold,ANY,missing,numeric-method
setMethod(
  f = "replace_threshold",
  signature = signature(x = "ANY", y = "missing", threshold = "numeric"),
  definition = function(x, threshold, value = 0, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, threshold = threshold,
                         value = value)
  }
)

# Negative =====================================================================
#' @export
#' @rdname replace_negative
#' @aliases replace_negative,numeric,numeric-method
setMethod(
  f = "replace_negative",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, value = 0) {
    replace_threshold(x, y, threshold = 0, value = value)
  }
)

#' @export
#' @rdname replace_negative
#' @aliases replace_negative,ANY,missing-method
setMethod(
  f = "replace_negative",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, value = 0) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, value = value)
  }
)
