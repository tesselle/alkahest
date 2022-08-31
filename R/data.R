# DATA

#' Gamma-Ray Spectrometry
#'
#' @format A [`data.frame`] with 1024 rows (channels) and 2 variables.
#'  \describe{
#'    \item{energy}{(keV)}
#'    \item{count}{}
#'  }
#' @examples
#' data("LaBr")
#' plot(LaBr, type = "l", xlab = "Energy (keV)", ylab = "Count")
#' @family datasets
#' @keywords datasets
"LaBr"

#' Gamma-Ray Spectrometry
#'
#' @format A [`data.frame`] with 8192 rows (channels) and 2 variables.
#'  \describe{
#'    \item{energy}{(keV)}
#'    \item{count}{}
#'  }
#' @examples
#' data("BEGe")
#' plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
#' @family datasets
#' @keywords datasets
"BEGe"
