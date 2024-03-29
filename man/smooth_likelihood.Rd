% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/smooth.R
\docType{methods}
\name{smooth_likelihood}
\alias{smooth_likelihood}
\alias{smooth_likelihood-method}
\alias{smooth_likelihood,numeric,numeric-method}
\alias{smooth_likelihood,ANY,missing-method}
\title{Penalized Likelihood Smoothing}
\usage{
smooth_likelihood(x, y, ...)

\S4method{smooth_likelihood}{numeric,numeric}(x, y, lambda, d = 2, SE = FALSE, progress = interactive())

\S4method{smooth_likelihood}{ANY,missing}(x, lambda, d = 2, SE = FALSE, progress = interactive())
}
\arguments{
\item{x, y}{A \code{\link{numeric}} vector. If \code{y} is missing, an attempt is made to
interpret \code{x} in a suitable way (see \code{\link[grDevices:xy.coords]{grDevices::xy.coords()}}).}

\item{...}{Currently not used.}

\item{lambda}{An \code{\link{integer}} giving the smoothing parameter. The larger
\code{lambda} is, the smoother the curve.}

\item{d}{An \code{\link{integer}} specifying the order of the penalty.}

\item{SE}{A \code{\link{logical}} scalar: should standard errors be returned?}

\item{progress}{A \code{\link{logical}} scalar: should a progress bar be displayed?}
}
\value{
Returns a \code{\link{list}} with two components \code{x} and \code{y}.
}
\description{
Penalized Likelihood Smoothing
}
\note{
\pkg{Matrix} is required.
}
\examples{
\dontrun{
## X-ray diffraction
data("XRD")

## Subset from 20 to 40 degrees
XRD <- signal_select(XRD, from = 20, to = 40)

## Plot diffractogram
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")

## Penalized likelihood smoothing
lambda <- seq(from = 1, to = 8, length.out = 40)
lambda <- 10^lambda

likelihood <- smooth_likelihood(XRD, lambda = lambda, d = 3)
lines(likelihood, col = "red")

## Strip ka2
ka2 <- ka2_strip_penalized(XRD, lambda = lambda, tau = 0.5, nseg = 1)
lines(ka2, col = "blue")
}
}
\references{
de Rooi, J. J., van der Pers, N. M., Hendrikx, R. W. A., Delhez, R.,
Böttger A. J. and Eilers, P. H. C. (2014). Smoothing of X-ray diffraction
data and Ka2 elimination using penalized likelihood and the composite link
model. \emph{Journal of Applied Crystallography}, 47: 852-860.
\doi{10.1107/S1600576714005809}
}
\seealso{
Other smoothing methods: 
\code{\link{smooth_loess}()},
\code{\link{smooth_rectangular}()},
\code{\link{smooth_savitzky}()},
\code{\link{smooth_triangular}()},
\code{\link{smooth_whittaker}()}
}
\author{
J. J. de Rooi \emph{et al.} (original R code).
}
\concept{smoothing methods}
