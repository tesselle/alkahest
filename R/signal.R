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

# Shift ========================================================================
#' @export
#' @rdname signal_shift
#' @aliases signal_shift,numeric,numeric-method
setMethod(
  f = "signal_shift",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, lag) {
    x <- x + lag
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname signal_shift
#' @aliases signal_shift,ANY,missing-method
setMethod(
  f = "signal_shift",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, y, lag) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lag = lag)
  }
)

# Baseline correction ==========================================================
#' @export
#' @rdname signal_correct
#' @aliases signal_correct,numeric,numeric-method
setMethod(
  f = "signal_correct",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, method = c("linear", "rubberband", "SNIP"), ...) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)

    ## Get method
    f <- switch(
      method,
      linear = baseline_linear,
      rubberband = baseline_rubberband,
      SNIP = baseline_snip
    )

    ## Baseline estimation
    bsl <- f(x = x, y = y, ...)

    xy <- list(x = x, y = y - bsl$y)
    attr(xy, "method") <- method
    xy
  }
)

#' @export
#' @rdname signal_correct
#' @aliases signal_correct,ANY,missing-method
setMethod(
  f = "signal_correct",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, method = c("linear", "rubberband", "SNIP"), ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, method = method, ...)
  }
)
