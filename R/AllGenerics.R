# GENERIC METHODS

# Baseline =====================================================================
#' Linear Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param from An [`numeric`] value giving the first data point (in `x` unit)
#'  to be used for linear interpolation.
#' @param to An [`integer`] value giving the last data point (in `x` unit)
#'  to be used for linear interpolation.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_linear-method
setGeneric(
  name = "baseline_linear",
  def = function(x, y, ...) standardGeneric("baseline_linear"),
  valueClass = "list"
)

#' Rubberband Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param noise A length-one [`numeric`] vector giving the noise level.
#'  Only used if `method` is "`rubberband`".
#' @param spline A [`logical`] scalar: should spline interpolation through the
#'  support points be used instead of linear interpolation?
#'  Only used if `method` is "`rubberband`".
#' @param ... Extra arguments to be passed to [stats::smooth.spline()].
#' @details
#'  A convex envelope of the spectrum is determined and the
#'  baseline is estimated as the part of the convex envelope lying below the
#'  spectrum. Note that the rubber band does not enter the concave regions
#'  (if any) of the spectrum.
#' @note
#'  `baseline_rubberband()` is slightly modified from C. Beleites'
#'  [hyperSpec::spc.rubberband()].
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @example inst/examples/ex-baseline.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_rubberband-method
setGeneric(
  name = "baseline_rubberband",
  def = function(x, y, ...) standardGeneric("baseline_rubberband"),
  valueClass = "list"
)

#' SNIP Baseline Estimation
#'
#' Sensitive Nonlinear Iterative Peak clipping algorithm.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param LLS A [`logical`] scalar: should the LLS operator be applied on `x`
#'  before employing SNIP algorithm? Only used if `method` is "`SNIP`".
#' @param decreasing A [`logical`] scalar: should a decreasing clipping window
#'  be used?
#' @param n An [`integer`] value giving the number of iterations.
#'  Only used if `method` is "`SNIP`".
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @references
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


#  Liland, K. H. (2015). 4S Peak Filling - baseline estimation by iterative
#  mean suppression. *MethodsX*, 2, 135-140. \doi{10.1016/j.mex.2015.02.009}.
# @rdname baseline
# @aliases baseline_peakfilling-method
# setGeneric(
#   name = "baseline_peakfilling",
#   def = function(x, y, ...) standardGeneric("baseline_peakfilling"),
#   valueClass = "list"
# )

# Integrate ====================================================================
#' Trapezoidal Rule
#'
#' Approximates the definite integral by using the trapezoidal rule.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-integrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family integration methods
#' @aliases signal_integrate-method
setGeneric(
  name = "integrate_trapezoid",
  def = function(x, y, ...) standardGeneric("integrate_trapezoid")
)

# Peaks ========================================================================
#' Find Peaks
#'
#' Finds local maxima in sequential data.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param method A [`character`] string specifying the method to be used for
#'  background noise estimation (see below).
#' @param SNR An [`integer`] giving the signal-to-noise-ratio for peak detection
#'  (see below).
#' @param m An odd [`integer`] giving the half window size.
#'  If `NULL`, 5% of the data points is used as the half window size.
#' @param ... Extra parameters to be passed to internal methods.
#' @details
#'  A local maximum has to be the highest one in the given window and has to be
#'  higher than \eqn{SNR \times noise}{SNR * noise} to be recognized as peak.
#'
#'  The following methods are available for noise estimation:
#'  \describe{
#'   \item{`MAD`}{Median Absolute Deviation.}
#'  }
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @note
#'  Adapted from Stasia Grinberg's
#'  [`findPeaks`](https://github.com/stas-g/findPeaks) function.
#' @example inst/examples/ex-peaks.R
#' @author N. Frerebeau
#' @docType methods
#' @family peaks detection methods
#' @aliases peaks_find-method
setGeneric(
  name = "peaks_find",
  def = function(x, y, ...) standardGeneric("peaks_find")
)

#' Half-Width at Half-Maximum
#'
#' Estimates the Half-Width at Half-Maximum (FWHM) for a given peak.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param center A [`numeric`] value giving the peak position in `x` units.
#' @param ... Currently not used.
#' @return A [`numeric`] value.
#' @details
#'  It tries to get the smallest possible estimate.
#' @example inst/examples/ex-peaks.R
#' @author N. Frerebeau
#' @docType methods
#' @family peaks detection methods
#' @aliases peaks_fwhm-method
setGeneric(
  name = "peaks_fwhm",
  def = function(x, y, ...) standardGeneric("peaks_fwhm")
)

# Replace ======================================================================
#' Replace Values Below a Given Threshold
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param threshold A [`numeric`] value or a [`function`] that takes a numeric
#'  vector as argument and returns a single numeric value.
#' @param value A [`numeric`] value to replace values below `threshold`.
#' @param ... Extra parameters to be passed to `threshold`.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @docType methods
#' @family replacement methods
#' @aliases replace_threshold-method
setGeneric(
  name = "replace_threshold",
  def = function(x, y, threshold, ...) standardGeneric("replace_threshold")
)

#' Replace Negative Values
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param value A [`numeric`] value to replace negative values.
#' @param ... Extra parameters to be passed to `threshold`.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @docType methods
#' @family replacement methods
#' @aliases replace_negative-method
setGeneric(
  name = "replace_negative",
  def = function(x, y, ...) standardGeneric("replace_negative")
)

# Scale ========================================================================
#' Rescales intensities to sum to a specified value
#'
#' Rescales intensities to sum to a specified value.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param total A legnth-one [`numeric`] vector specifying the output total.
#'  Defaults to 1, i.e. normalizes by total intensity.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-rescale.R
#' @author N. Frerebeau
#' @docType methods
#' @family normalization methods
#' @aliases rescale_total-method
setGeneric(
  name = "rescale_total",
  def = function(x, y, ...) standardGeneric("rescale_total"),
  valueClass = "list"
)

#' Rescales intensities to have specified minimum and maximum
#'
#' Rescales intensities to have specified minimum and maximum.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param min A legnth-one [`numeric`] vector specifying the output minimum.
#' @param max A legnth-one [`numeric`] vector specifying the output maximum.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-rescale.R
#' @author N. Frerebeau
#' @docType methods
#' @family normalization methods
#' @aliases rescale_range-method
setGeneric(
  name = "rescale_range",
  def = function(x, y, ...) standardGeneric("rescale_range"),
  valueClass = "list"
)

#' @rdname rescale_range
#' @aliases rescale_min-method
setGeneric(
  name = "rescale_min",
  def = function(x, y, ...) standardGeneric("rescale_min"),
  valueClass = "list"
)

#' @rdname rescale_range
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
#'  Transformation of intensities can be used to improve the identification of
#'  peaks with a low signal-to-noise ratio.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-transform.R
#' @author N. Frerebeau
#' @docType methods
#' @family normalization methods
#' @aliases rescale_transform-method
setGeneric(
  name = "rescale_transform",
  def = function(x, y, ...) standardGeneric("rescale_transform"),
  valueClass = "list"
)

# Signal =======================================================================
#' Baseline Correction
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param method A [`character`] string specifying the method for baseline
#'  estimation. It must be one of "`linear`", "`rubberband`" or "`SNIP`".
#'  Any unambiguous substring can be given.
#' @param ... Extra arguments to be passed to `baseline_*()`.
#' @example inst/examples/ex-baseline.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_correct-method
setGeneric(
  name = "signal_correct",
  def = function(x, y, ...) standardGeneric("signal_correct"),
  valueClass = "list"
)

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
#' Rectangular Smoothing
#'
#' Unweighted sliding-average or rectangular Smoothing.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the number of adjacent points to be used.
#' @param ... Currently not used.
#' @details
#'  It replaces each point in the signal with the average of \eqn{m} adjacent
#'  points.
#'
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, progressively smaller smooths are used at
#'  the ends of the spectrum.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family smoothing methods
#' @aliases smooth_rectangular-method
setGeneric(
  name = "smooth_rectangular",
  def = function(x, y, ...) standardGeneric("smooth_rectangular"),
  valueClass = "list"
)

#' Triangular Smoothing
#'
#' Weighted sliding-average or triangular smoothing.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the number of adjacent points to be used.
#' @param ... Currently not used.
#' @details
#'  It replaces each point in the signal with the weighted mean of \eqn{m}
#'  adjacent points.
#'
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, progressively smaller smooths are used at
#'  the ends of the spectrum.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family smoothing methods
#' @aliases smooth_triangular-method
setGeneric(
  name = "smooth_triangular",
  def = function(x, y, ...) standardGeneric("smooth_triangular"),
  valueClass = "list"
)

#' Loess Smoothing
#'
#' Smoothes intensities by loess fitting.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param span An [`integer`] specifying the degree of smoothing (see
#'  [stats::loess()]).
#' @param ... Extra arguments to be passed to [stats::loess()].
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family smoothing methods
#' @aliases smooth_loess-method
setGeneric(
  name = "smooth_loess",
  def = function(x, y, ...) standardGeneric("smooth_loess"),
  valueClass = "list"
)

#' Savitzky-Golay Filter
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the number of adjacent points to be used.
#' @param p An [`integer`] giving the degree of the polynomial to be used.
#' @param ... Currently not used.
#' @details
#'  This method is based on the least-squares fitting of polynomials to
#'  segments of \eqn{m} adjacent points.
#'
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the spectrum for which a complete \eqn{m}-width smooth cannot be
#'  calculated. To prevent data loss, the original \eqn{(m - 1) / 2} points at
#'  the ends of the spectrum are preserved.
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
#' @family smoothing methods
#' @aliases smooth_savitzky-method
setGeneric(
  name = "smooth_savitzky",
  def = function(x, y, ...) standardGeneric("smooth_savitzky"),
  valueClass = "list"
)
