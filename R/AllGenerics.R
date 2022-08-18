# GENERIC METHODS

# Baseline =====================================================================
#' Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param LLS A [`logical`] scalar: should the LLS operator be applied on `x`
#'  before employing SNIP algorithm? Only used if `method` is "`SNIP`".
#' @param decreasing A [`logical`] scalar: should a decreasing clipping window
#'  be used? Only used if `method` is "`SNIP`".
#' @param n An [`integer`] value giving the number of iterations.
#'  Only used if `method` is "`SNIP`".
#' @param noise A length-one [`numeric`] vector giving the noise level.
#'  Only used if `method` is "`rubberband`".
#' @param spline A [`logical`] scalar: should spline interpolation through the
#'  support points be used instead of linear interpolation?
#'  Only used if `method` is "`rubberband`".
#' @param from An [`numeric`] value giving the first data point (in `x` unit)
#'  to be used for linear interpolation.
#' @param to An [`integer`] value giving the last data point (in `x` unit)
#'  to be used for linear interpolation.
#' @param ... Extra parameters to be passed to further methods.
#' @details
#'  The following methods are available for baseline estimation:
#'  \describe{
#'   \item{`SNIP`}{Sensitive Nonlinear Iterative Peak clipping algorithm.}
#'   \item{`rubberband`}{A convex envelope of the spectrum is determined and the
#'   baseline is estimated as the part of the convex envelope lying below the
#'   spectrum. Note that the rubber band does not enter the concave regions
#'   (if any) of the spectrum.}
#'   \item{`linear`}{Linear baseline estimation.}
#'  }
#' @note
#'  `baseline_rubberband()` is slightly modified from C. Beleites'
#'  [hyperSpec::spc.rubberband()].
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @references
#'  Liland, K. H. (2015). 4S Peak Filling - baseline estimation by iterative
#'  mean suppression. *MethodsX*, 2, 135-140. \doi{10.1016/j.mex.2015.02.009}.
#'
#'  Morháč, M., Kliman, J., Matoušek, V., Veselský, M. & Turzo, I. (1997).
#'  Background elimination methods for multidimensional gamma-ray spectra.
#'  *Nuclear Instruments and Methods in Physics Research Section A:
#'  Accelerators, Spectrometers, Detectors and Associated Equipment*, 401(1),
#'  p. 113-132. \doi{10.1016/S0168-9002(97)01023-1}
#'
#'  Morháč, M. & Matoušek, V. (2008). Peak Clipping Algorithms for Background
#'  Estimation in Spectroscopic Data. *Applied Spectroscopy*, 62(1), p. 91-106.
#'  \doi{10.1366/000370208783412762}
#'
#'  Ryan, C. G., Clayton, E., Griffin, W. L., Sie, S. H. & Cousens, D. R.
#'  (1988). SNIP, a statistics-sensitive background treatment for the
#'  quantitative analysis of PIXE spectra in geoscience applications.
#'  *Nuclear Instruments and Methods in Physics Research Section B:
#'  Beam Interactions with Materials and Atoms*, 34(3), p. 396-402.
#'  \doi{10.1016/0168-583X(88)90063-8}
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @name baseline
#' @rdname baseline
NULL

#' @rdname baseline
#' @aliases baseline_linear-method
setGeneric(
  name = "baseline_linear",
  def = function(x, y, ...) standardGeneric("baseline_linear"),
  valueClass = "list"
)

#' @rdname baseline
#' @aliases baseline_rubberband-method
setGeneric(
  name = "baseline_rubberband",
  def = function(x, y, ...) standardGeneric("baseline_rubberband"),
  valueClass = "list"
)

#' @rdname baseline
#' @aliases baseline_snip-method
setGeneric(
  name = "baseline_snip",
  def = function(x, y, ...) standardGeneric("baseline_snip"),
  valueClass = "list"
)

# @rdname baseline
# @aliases baseline_rollingball-method
# setGeneric(
#   name = "baseline_rollingball",
#   def = function(x, y, ...) standardGeneric("baseline_rollingball"),
#   valueClass = "list"
# )

# @rdname baseline
# @aliases baseline_peakfilling-method
# setGeneric(
#   name = "baseline_peakfilling",
#   def = function(x, y, ...) standardGeneric("baseline_peakfilling"),
#   valueClass = "list"
# )

# Scale ========================================================================
#' Normalize Intensities
#'
#' @description
#'  * `rescale_total()` rescales intensities to sum to a specified value.
#'  * `rescale_range()`, `rescale_min()` and`rescale_max()` rescales intensities
#'  to have specified minimum and maximum.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param total A legnth-one [`numeric`] vector specifying the output total.
#' @param min A legnth-one [`numeric`] vector specifying the output minimum.
#' @param max A legnth-one [`numeric`] vector specifying the output maximum.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-rescale.R
#' @author N. Frerebeau
#' @docType methods
#' @family normalizing methods
#' @name rescale
#' @rdname rescale
NULL

#' @rdname rescale
#' @aliases rescale_total-method
setGeneric(
  name = "rescale_total",
  def = function(x, y, ...) standardGeneric("rescale_total"),
  valueClass = "list"
)

#' @rdname rescale
#' @aliases rescale_range-method
setGeneric(
  name = "rescale_range",
  def = function(x, y, ...) standardGeneric("rescale_range"),
  valueClass = "list"
)

#' @rdname rescale
#' @aliases rescale_min-method
setGeneric(
  name = "rescale_min",
  def = function(x, y, ...) standardGeneric("rescale_min"),
  valueClass = "list"
)

#' @rdname rescale
#' @aliases rescale_max-method
setGeneric(
  name = "rescale_max",
  def = function(x, y, ...) standardGeneric("rescale_max"),
  valueClass = "list"
)

#' Transform Intensities
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param f A [`function`] that takes a `numeric` vector of intensities as
#'  argument and returns a `numeric` vector.
#' @param ... Extra arguments to be passed to `f`.
#' @details
#'  The stabilization step aims at improving the identification of peaks with a
#'  low signal-to-noise ratio. This particularly targets higher energy peaks.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family signal processing methods
#' @rdname transform
#' @aliases signal_transform-method
setGeneric(
  name = "signal_transform",
  def = function(x, y, ...) standardGeneric("signal_transform"),
  valueClass = "list"
)

# Signal =======================================================================
#' Subset
#'
#' @description
#'  * `signal_select()` allows to subset by values of `x`.
#'  * `signal_slice()` allows to subset by position along `x`.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param subset An [`integer`] vector giving either positive values to keep,
#'  or negative values to drop. The values provided must be either all
#'  positive or all negative (coerced to integer as by [as.integer()]).
#' @param from,to A [`numeric`] value giving the first and last value (in `x`
#'  unit) to be selected.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-subset.R
#' @docType methods
#' @family signal processing methods
#' @name subset
#' @rdname subset
NULL

#' @rdname subset
#' @aliases signal_select-method
setGeneric(
  name = "signal_select",
  def = function(x, y, ...) standardGeneric("signal_select"),
  valueClass = "list"
)

#' @rdname subset
#' @aliases signal_slice-method
setGeneric(
  name = "signal_slice",
  def = function(x, y, ...) standardGeneric("signal_slice"),
  valueClass = "list"
)

# Smooth =======================================================================
#' Smooth
#'
#' Smoothes intensities.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the number of adjacent points to be used.
#' @param p An [`integer`] giving the polynomial degree.
#' @param ... Currently not used.
#' @details
#'  The following smoothing methods are available:
#'  \describe{
#'   \item{`rectangular`}{Unweighted sliding-average or rectangular smooth.
#'   It replaces each point in the signal with the average of \eqn{m} adjacent
#'   points.}
#'   \item{`triangular`}{Weighted sliding-average or triangular smooth.
#'   It replaces each point in the signal with the weighted mean of \eqn{m}
#'   adjacent points.}
#'   \item{`savitzky`}{Savitzky-Golay filter. This method is based on the
#'   least-squares fitting of polynomials to segments of \eqn{m} adjacent
#'   points.}
#'  }
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, progressively smaller smooths are used at
#'  the ends of the spectrum by `smooth_rectangular()` and
#'  `smooth_triangular()`. If the Savitzky-Golay filter is used, the original
#'  \eqn{(m - 1) / 2} points at the ends of the spectrum are preserved.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @references
#'  Gorry, P. A. (1990). General Least-Squares Smoothing and Differentiation by
#'  the Convolution (Savitzky-Golay) Method. *Analytical Chemistry*, 62(6),
#'  p. 570-573. \doi{10.1021/ac00205a007}.
#'
#'  Savitzky, A. & Golay, M. J. E. (1964). Smoothing and Differentiation of
#'  Data by Simplified Least Squares Procedures. *Analytical Chemistry*,
#'  36(8), p. 1627-1639. \doi{10.1021/ac60214a047}.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family signal processing methods
#' @name smooth
#' @rdname smooth
NULL

#' @rdname smooth
#' @aliases smooth_rectangular-method
setGeneric(
  name = "smooth_rectangular",
  def = function(x, y, ...) standardGeneric("smooth_rectangular"),
  valueClass = "list"
)

#' @rdname smooth
#' @aliases smooth_triangular-method
setGeneric(
  name = "smooth_triangular",
  def = function(x, y, ...) standardGeneric("smooth_triangular"),
  valueClass = "list"
)

#' @rdname smooth
#' @aliases smooth_savitzky-method
setGeneric(
  name = "smooth_savitzky",
  def = function(x, y, ...) standardGeneric("smooth_savitzky"),
  valueClass = "list"
)
