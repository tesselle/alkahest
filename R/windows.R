# SLIDING WINDOWS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname window_sliding
#' @aliases window_sliding,integer,integer-method
setMethod(
  f = "window_sliding",
  signature = signature(n = "integer", m = "integer"),
  definition = function(n, m, i = NULL) {
    ## Validation
    assert_odd(m)

    ## Index
    k <- (m - 1) / 2
    x <- if (is.null(i)) seq_len(n) else i

    left <- x - k
    left <- ifelse(left > 0, left, 1)

    right <- x + k
    right <- ifelse(right < n, right, n)

    mapply(
      FUN = seq,
      from = left,
      to = right,
      MoreArgs = list(by = 1),
      SIMPLIFY = FALSE
    )
  }
)

#' @export
#' @rdname window_sliding
#' @aliases window_sliding,numeric,numeric-method
setMethod(
  f = "window_sliding",
  signature = signature(n = "numeric", m = "numeric"),
  definition = function(n, m, i = NULL) {
    n <- as.integer(n)
    m <- as.integer(m)
    methods::callGeneric(n, m, i = i)
  }
)

#' @export
#' @rdname window_tumbling
#' @aliases window_tumbling,integer,integer-method
setMethod(
  f = "window_tumbling",
  signature = signature(n = "integer", m = "integer"),
  definition = function(n, m, drop = FALSE) {
    k <- n %% m
    if (drop && k != 0) {
      n <- n - k
    }

    x <- seq_len(n)
    i <- cut(x, breaks = n / m, labels = FALSE)
    split(x, f = i)
  }
)

#' @export
#' @rdname window_tumbling
#' @aliases window_tumbling,numeric,numeric-method
setMethod(
  f = "window_tumbling",
  signature = signature(n = "numeric", m = "numeric"),
  definition = function(n, m, drop = FALSE) {
    n <- as.integer(n)
    m <- as.integer(m)
    methods::callGeneric(n, m, drop = drop)
  }
)
