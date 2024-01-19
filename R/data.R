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

#' Powder X-ray Diffraction
#'
#' @format A [`data.frame`] with 2989 rows and 2 variables.
#'  \describe{
#'    \item{theta}{}
#'    \item{count}{}
#'  }
#' @examples
#' data("XRD")
#' plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
#' @family datasets
#' @keywords datasets
"XRD"

#' Raman Spectroscopy
#'
#' @format A [`data.frame`] with 1182 rows and 2 variables.
#'  \describe{
#'    \item{shift}{Raman shift.}
#'    \item{intensity}{}
#'  }
#' @examples
#' data("Raman")
#' plot(Raman, type = "l", xlab = "Shift", ylab = "Intensity")
#' @family datasets
#' @keywords datasets
"Raman"
