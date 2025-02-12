# SCALING
#' @include AllGenerics.R
NULL

# Area =========================================================================
#' @export
#' @rdname rescale_area
#' @aliases rescale_area,numeric,numeric-method
setMethod(
  f = "rescale_area",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, method = c("rectangle", "trapezoid"), ...) {
    ## Validation
    assert_length(y, length(x))
    method <- match.arg(method, several.ok = FALSE)

    ## Get method
    f <- switch(
      method,
      rectangle = integrate_rectangle,
      trapezoid = integrate_trapezoid
    )

    ## Baseline estimation
    auc <- f(x = x, y = y, ...)
    y <- y / auc

    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname rescale_area
#' @aliases rescale_area,ANY,missing-method
setMethod(
  f = "rescale_area",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, method = c("rectangle", "trapezoid"), ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, method = method, ...)
  }
)

# Total ========================================================================
#' @export
#' @rdname rescale_total
#' @aliases rescale_total,numeric,numeric-method
setMethod(
  f = "rescale_total",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, total = 1) {
    assert_length(y, length(x))

    y <- (y * total) / sum(abs(y))
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname rescale_total
#' @aliases rescale_total,ANY,missing-method
setMethod(
  f = "rescale_total",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, total = 1) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, total = total)
  }
)

# Min-Max ======================================================================
#' @export
#' @rdname rescale_range
#' @aliases rescale_range,numeric,numeric-method
setMethod(
  f = "rescale_range",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, min = 0, max = 1) {
    ## Validation
    assert_length(y, length(x))
    if (min > max) {
      msg <- sprintf(tr_("%s (%g) must be lower than %s (%g)."),
                     sQuote("min"), min, sQuote("max"), max)
      stop(msg, call. = FALSE)
    }

    y <- (y - min(y)) / (max(y) - min(y)) * (max - min) + min
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname rescale_range
#' @aliases rescale_range,ANY,missing-method
setMethod(
  f = "rescale_range",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, min = 0, max = 1) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, min = min, max = max)
  }
)

#' @export
#' @rdname rescale_range
#' @aliases rescale_min,numeric,numeric-method
setMethod(
  f = "rescale_min",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, min = 0) {
    rescale_range(x, y, min = min, max = max(y))
  }
)

#' @export
#' @rdname rescale_range
#' @aliases rescale_min,ANY,missing-method
setMethod(
  f = "rescale_min",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, min = 0) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, min = min)
  }
)

#' @export
#' @rdname rescale_range
#' @aliases rescale_max,numeric,numeric-method
setMethod(
  f = "rescale_max",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, max = 1) {
    rescale_range(x, y, min = min(y), max = max)
  }
)

#' @export
#' @rdname rescale_range
#' @aliases rescale_max,ANY,missing-method
setMethod(
  f = "rescale_max",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, max = 1) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, max = max)
  }
)

# Transform ====================================================================
#' @export
#' @rdname rescale_transform
#' @aliases rescale_transform,numeric,numeric-method
setMethod(
  f = "rescale_transform",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, f, ...) {
    assert_length(y, length(x))

    y <- f(y, ...)
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname rescale_transform
#' @aliases rescale_transform,ANY,missing-method
setMethod(
  f = "rescale_transform",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, f, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, f = f, ...)
  }
)

# SNV ==========================================================================
#' @export
#' @rdname rescale_snv
#' @aliases rescale_snv,numeric,numeric-method
setMethod(
  f = "rescale_snv",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, ...) {
    ## Validation
    assert_length(y, length(x))

    y <- (y - mean(y)) / stats::sd(y)

    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname rescale_snv
#' @aliases rescale_snv,ANY,missing-method
setMethod(
  f = "rescale_snv",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, ...)
  }
)
