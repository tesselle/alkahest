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
