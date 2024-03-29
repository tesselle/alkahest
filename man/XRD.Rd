% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{XRD}
\alias{XRD}
\title{Powder X-ray Diffraction}
\format{
A \code{\link{data.frame}} with 2989 rows and 2 variables.
\describe{
\item{theta}{}
\item{count}{}
}
}
\usage{
XRD
}
\description{
Powder X-ray Diffraction
}
\examples{
data("XRD")
plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
}
\seealso{
Other datasets: 
\code{\link{BEGe}},
\code{\link{LaBr}},
\code{\link{Raman}}
}
\concept{datasets}
\keyword{datasets}
