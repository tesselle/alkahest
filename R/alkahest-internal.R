# HELPERS

## https://michaelchirico.github.io/potools/articles/developers.html
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-alkahest"))
}

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

#' Check Odd Numbers
#'
#' @param x A [`numeric`] vector.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @keywords internal
#' @noRd
assert_odd <- function(x) {
  arg <- deparse(substitute(x))
  if (any(round(x) %% 2 == 0)) {
    msg <- sprintf("%s must be an odd integer (%g).", sQuote(arg), x)
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

#' Check Object Length
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @keywords internal
#' @noRd
assert_length <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (length(x) != expected) {
    str <- "%s must be of length %d; not %d."
    msg <- sprintf(str, sQuote(arg), expected, length(x))
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

assert_Matrix <- function() {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    msg <- "The Matrix package is required. Please install it."
    stop(msg, call. = FALSE)
  }
  invisible()
}
