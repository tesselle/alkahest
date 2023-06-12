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
