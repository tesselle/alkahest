# HELPERS

#' Find the Nearest Value in a Vector
#'
#' @param x A [`numeric`] vector.
#' @param value A [`numeric`] value.
#' @return An [`integer`].
#' @keywords internal
#' @noRd
which_nearest <- function(x, value) {
  which.min(abs(x - value))
}

#' Check Data
#'
#' @param x A [`numeric`] vector.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @keywords internal
#' @noRd
assert_odd <- function(x) {
  arg <- deparse(substitute(x))
  if (round(x) %% 2 == 0) {
    msg <- sprintf("%s must be an odd integer (%g).", sQuote(arg), x)
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

#' Sliding Windows
#'
#' @param n A [`numeric`] vector.
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used).
#' @param i An [`integer`] vector.
#' @return A [`list`] of [`integer`].
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  used at both ends of the data series.
#' @keywords internal
#' @noRd
which_window <- function(n, m, i = NULL) {
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
