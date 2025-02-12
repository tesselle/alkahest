# SIGNAL PROCESSING
#' @include AllGenerics.R
NULL

# Bind =========================================================================
#' @export
#' @rdname signal_bind
#' @aliases signal_bind,ANY-method
setMethod(
  f = "signal_bind",
  signature = "ANY",
  definition = function(...) {

    signal <- list(...)
    coords <- lapply(X = signal, FUN = grDevices::xy.coords)

    y <- lapply(X = coords, FUN = `[[`, i = "y")
    ny <- lengths(y)
    if (any(ny != mean(ny))) {
      msg <- tr_("All objects must have the same number of data points.")
      stop(msg, call. = FALSE)
    }
    y <- do.call(rbind, y)

    ## Get names
    subst <- substitute(list(...))[-1]
    arg_names <- vapply(X = subst, FUN = deparse, FUN.VALUE = character(1))
    rownames(y) <- arg_names
    y
  }
)

# Mean =========================================================================
#' @export
#' @rdname signal_mean
#' @aliases signal_mean,ANY-method
setMethod(
  f = "signal_mean",
  signature = "ANY",
  definition = function(...) {

    signal <- list(...)
    coords <- lapply(X = signal, FUN = grDevices::xy.coords)

    x <- lapply(X = coords, FUN = `[[`, i = "x")
    nx <- lengths(x)

    y <- lapply(X = coords, FUN = `[[`, i = "y")
    ny <- lengths(y)

    if (any(nx != mean(nx)) | any(ny != mean(ny))) {
      msg <- tr_("All objects must have the same number of data points.")
      stop(msg, call. = FALSE)
    }

    x <- do.call(cbind, x)
    x <- rowMeans(x)
    y <- do.call(cbind, y)
    y <- rowMeans(y)

    xy <- list(x = x, y = y)
    xy
  }
)

# Subset =======================================================================
#' @export
#' @rdname subset
#' @aliases signal_select,numeric,numeric-method
setMethod(
  f = "signal_select",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, from, to) {
    assert_length(y, length(x))

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
  signature = c(x = "ANY", y = "missing"),
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
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, subset) {
    assert_length(y, length(x))

    i <- as.integer(subset)

    if (!all(i > 0) & !all(i < 0)) {
      msg <- tr_("A vector of strictly positive or negative integers is expected.")
      stop(msg, call. = FALSE)
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
  signature = c(x = "ANY", y = "missing"),
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
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, lag) {
    assert_length(y, length(x))

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
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, lag) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lag = lag)
  }
)

# Drift ========================================================================
#' @export
#' @rdname signal_drift
#' @aliases signal_drift,numeric,numeric,numeric-method
setMethod(
  f = "signal_drift",
  signature = c(x = "numeric", y = "numeric", lag = "numeric"),
  definition = function(x, y, lag) {
    assert_length(y, length(x))

    y <- y + lag
    xy <- list(x = x, y = y)
    xy
  }
)

#' @export
#' @rdname signal_drift
#' @aliases signal_drift,ANY,missing,ANY-method
setMethod(
  f = "signal_drift",
  signature = c(x = "ANY", y = "missing", lag = "ANY"),
  definition = function(x, lag, subtract = FALSE) {
    xy <- grDevices::xy.coords(x)
    zz <- grDevices::xy.coords(lag)
    lag <- if (subtract) -zz$y else zz$y
    methods::callGeneric(x = xy$x, y = xy$y, lag = lag)
  }
)

# Correct ======================================================================
#' @export
#' @rdname signal_correct
#' @aliases signal_correct,numeric,numeric-method
setMethod(
  f = "signal_correct",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, method = c("linear", "polynomial", "asls",
                                         "rollingball", "rubberband",
                                         "SNIP", "4S"), ...) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)

    ## Get method
    f <- switch(
      method,
      asls = baseline_asls,
      linear = baseline_linear,
      polynomial = baseline_polynomial,
      rollingball = baseline_rollingball,
      rubberband = baseline_rubberband,
      SNIP = baseline_snip,
      `4S` = baseline_peakfilling
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
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, method = c("linear", "polynomial", "asls",
                                      "rollingball", "rubberband",
                                      "SNIP", "4S"), ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, method = method, ...)
  }
)
