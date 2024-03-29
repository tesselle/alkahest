% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/rescale.R
\docType{methods}
\name{rescale_range}
\alias{rescale_range}
\alias{rescale_range-method}
\alias{rescale_min}
\alias{rescale_min-method}
\alias{rescale_max}
\alias{rescale_max-method}
\alias{rescale_range,numeric,numeric-method}
\alias{rescale_range,ANY,missing-method}
\alias{rescale_min,numeric,numeric-method}
\alias{rescale_min,ANY,missing-method}
\alias{rescale_max,numeric,numeric-method}
\alias{rescale_max,ANY,missing-method}
\title{Rescales intensities to have specified minimum and maximum}
\usage{
rescale_range(x, y, ...)

rescale_min(x, y, ...)

rescale_max(x, y, ...)

\S4method{rescale_range}{numeric,numeric}(x, y, min = 0, max = 1)

\S4method{rescale_range}{ANY,missing}(x, min = 0, max = 1)

\S4method{rescale_min}{numeric,numeric}(x, y, min = 0)

\S4method{rescale_min}{ANY,missing}(x, min = 0)

\S4method{rescale_max}{numeric,numeric}(x, y, max = 1)

\S4method{rescale_max}{ANY,missing}(x, max = 1)
}
\arguments{
\item{x, y}{A \code{\link{numeric}} vector. If \code{y} is missing, an attempt is made to
interpret \code{x} in a suitable way (see \code{\link[grDevices:xy.coords]{grDevices::xy.coords()}}).}

\item{...}{Currently not used.}

\item{min}{A legnth-one \code{\link{numeric}} vector specifying the output minimum.}

\item{max}{A legnth-one \code{\link{numeric}} vector specifying the output maximum.}
}
\value{
Returns a \code{\link{list}} with two components \code{x} and \code{y}.
}
\description{
Rescales intensities to have specified minimum and maximum.
}
\examples{
## gamma-ray spectrometry
data("BEGe")

## Subset from 2.75 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## Plot spectrum
plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Normalize by area under the curve
BEGe_area <- rescale_area(BEGe)
plot(BEGe_area, type = "l", xlab = "Energy (keV)", ylab = "Count")
integrate_rectangle(BEGe)
integrate_rectangle(BEGe_area)

## Rescale so that intensities sum to 1
BEGe_total <- rescale_total(BEGe, total = 1)
plot(BEGe_total, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Rescale intensities to 0-1
BEGe_range <- rescale_range(BEGe, min = 0, max = 1)
plot(BEGe_range, type = "l", xlab = "Energy (keV)", ylab = "Count")
}
\seealso{
Other normalization methods: 
\code{\link{rescale_area}()},
\code{\link{rescale_snv}()},
\code{\link{rescale_total}()},
\code{\link{rescale_transform}()}
}
\author{
N. Frerebeau
}
\concept{normalization methods}
