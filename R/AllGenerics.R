# GENERIC METHODS

# Baseline =====================================================================
#' Linear Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param points A [`numeric`] vector specifying the data points to be used in
#'  the fitting process (in `x` unit).
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @example inst/examples/ex-baseline-linear.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_linear-method
setGeneric(
  name = "baseline_linear",
  def = function(x, y, ...) standardGeneric("baseline_linear"),
  valueClass = "list"
)

#' Polynomial Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param d An [`integer`] giving the degree of the polynomial. Must be less
#'  than the number of unique points.
#' @param tolerance A [`numeric`] scalar giving the tolerance of difference
#'  between iterations.
#' @param stop An [`integer`] giving the stopping rule (i.e. maximum number of
#'  iterations).
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @references
#'  Lieber, C. A. and Mahadevan-Jansen, A. (2003). Automated Method for
#'  Subtraction of Fluorescence from Biological Raman Spectra. *Applied
#'  Spectroscopy*, 57(11): 1363-67. \doi{10.1366/000370203322554518}.
#' @example inst/examples/ex-baseline-polynomial.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_polynomial-method
setGeneric(
  name = "baseline_polynomial",
  def = function(x, y, ...) standardGeneric("baseline_polynomial"),
  valueClass = "list"
)

#' Rolling Ball Baseline Estimation
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used; see [`window_sliding()`]) for
#'  minimization/maximization.
#' @param s An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used; see [`window_sliding()`]) for smoothing.
#' @param ... Currently not used.
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  used at both ends of the data series.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @seealso [signal_correct()]
#' @references
#'  Kneen, M. A. and Annegarn, H. J. (1996). Algorithm for Fitting XRF, SEM and
#'  PIXE X-Ray Spectra Backgrounds. *Nuclear Instruments and Methods in Physics
#'  Research Section B: Beam Interactions with Materials and Atoms*,
#'  109/110: 209-213. \doi{10.1016/0168-583X(95)00908-6}.
#' @example inst/examples/ex-baseline-rollingball.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_rollingball-method
setGeneric(
 name = "baseline_rollingball",
 def = function(x, y, ...) standardGeneric("baseline_rollingball"),
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
#' @example inst/examples/ex-baseline-rubberband.R
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
#' @example inst/examples/ex-baseline-snip.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_snip-method
setGeneric(
  name = "baseline_snip",
  def = function(x, y, ...) standardGeneric("baseline_snip"),
  valueClass = "list"
)

#' 4S Peak Filling
#'
#' Baseline estimation by iterative mean suppression.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param n An [`integer`] value giving the number of iterations.
#' @param m An odd [`integer`] giving the half window size.
#' @param by A length-one [`numeric`] vector givging the umber of buckets to
#'  divide `x` into.
#' @param lambda An [`integer`] giving the smoothing parameter. The larger
#'  `lambda` is, the smoother the curve (see [smooth_whittaker()]).
#' @param d An [`integer`] specifying the order of the penalty (see
#'  [smooth_whittaker()]).
#' @param sparse A [`logical`] scalar: should sparse matrices be used for
#'  computation (see [smooth_whittaker()])? If `TRUE`, \pkg{Matrix} is required.
#' @param ... Currently not used.
#' @seealso [signal_correct()], [smooth_whittaker()]
#' @references
#'  Liland, K. H. (2015). 4S Peak Filling - baseline estimation by iterative
#'  mean suppression. *MethodsX*, 2, 135-140. \doi{10.1016/j.mex.2015.02.009}.
#' @example inst/examples/ex-baseline-peakfilling.R
#' @author N. Frerebeau
#' @docType methods
#' @family baseline estimation methods
#' @aliases baseline_peakfilling-method
setGeneric(
  name = "baseline_peakfilling",
  def = function(x, y, ...) standardGeneric("baseline_peakfilling"),
  valueClass = "list"
)

# Integrate ====================================================================
#' Rectangle Rule
#'
#' Approximates the definite integral by using the rectangle rule.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param right A [`logical`] scalar: should the right rule be used instead of
#'  the left rule?
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-integrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family integration methods
#' @aliases integrate_rectangle-method
setGeneric(
  name = "integrate_rectangle",
  def = function(x, y, ...) standardGeneric("integrate_rectangle")
)

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
#' @aliases integrate_trapezoid-method
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
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used).
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
#'
#'  Note that to improve peak detection, it may be helpful to smooth the data
#'  and remove the baseline beforehand.
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  used at both ends of the data series.
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
  def = function(x, y, ...) standardGeneric("peaks_find"),
  valueClass = "list"
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
#' @param threshold A [`numeric`] value or a [`function`] that takes a `numeric`
#'  vector as argument and returns a single `numeric` value.
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
  def = function(x, y, threshold, ...) standardGeneric("replace_threshold"),
  valueClass = "list"
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
  def = function(x, y, ...) standardGeneric("replace_negative"),
  valueClass = "list"
)

# Resample =====================================================================
#' Bin
#'
#' Averages `x` values and applies a function to the corresponding `y` values.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param by An [`integer`] specifying the binning ratio (i.e. the number of
#'  points to be grouped together; see [`window_tumbling()`]).
#' @param f A [`function`] that takes a `numeric` vector of intensities as
#'  argument and returns a single `numeric` vector. Used to estimate the local
#'  representative value in each bin (defaults to [sum()]; see examples).
#' @param ... Extra parameters to be passed to `f`.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-resample.R
#' @docType methods
#' @family resampling methods
#' @aliases resample_bin-method
setGeneric(
  name = "resample_bin",
  def = function(x, y, ...) standardGeneric("resample_bin"),
  valueClass = "list"
)

#' Downsample
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param by An [`integer`] specifying the downsampling factor.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-resample.R
#' @docType methods
#' @family resampling methods
#' @aliases resample_down-method
setGeneric(
  name = "resample_down",
  def = function(x, y, ...) standardGeneric("resample_down"),
  valueClass = "list"
)

# Upsample
#
# @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
# @param by An [`integer`] specifying the upsampling factor.
# @param ... Currently not used.
# @details
#  The signal is upsampled by inserting \eqn{n - 1} zeros between samples.
# @return
#  Returns a [`list`] with two components `x` and `y`.
# @author N. Frerebeau
# @example inst/examples/ex-resample.R
# @docType methods
# @family resampling methods
# @aliases resample_interpolate-method
# setGeneric(
#   name = "resample_up",
#   def = function(x, y, ...) standardGeneric("resample_up"),
#   valueClass = "list"
# )

#' Linearly Interpolate
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param from A length-one [`numeric`] vector giving the starting value of the
#'  sequence where interpolation is to take place.
#' @param to A length-one [`numeric`] vector giving the end value of the
#'  sequence where interpolation is to take place.
#' @param by A length-one [`numeric`] vector specifying the increment of the
#'  sequence.
#' @param ... Extra arguments to be passed to [stats::approx()].
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-resample.R
#' @docType methods
#' @family resampling methods
#' @aliases resample_interpolate-method
setGeneric(
  name = "resample_interpolate",
  def = function(x, y, ...) standardGeneric("resample_interpolate"),
  valueClass = "list"
)

# Rescale ======================================================================
#' Normalize intensities by AUC
#'
#' Rescales intensities so that the area under the curve (AUC) is equal to 1.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param method A [`character`] string specifying the method for integration.
#'  It must be one of "`rectangle`" or "`trapezoid`".
#'  Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @example inst/examples/ex-rescale.R
#' @author N. Frerebeau
#' @docType methods
#' @family normalization methods
#' @aliases rescale_area-method
setGeneric(
  name = "rescale_area",
  def = function(x, y, ...) standardGeneric("rescale_area"),
  valueClass = "list"
)

#' Rescale intensities to sum to a specified value
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
#'  estimation. It must be one of "`linear`", "`rubberband`", "`SNIP`" or "`4S`"
#'  (see details). Any unambiguous substring can be given.
#' @param ... Extra arguments to be passed to `baseline_*()` (see details).
#' @details
#'  Available methods for baseline estimation:
#'  \describe{
#'   \item{`linear`}{Linear baseline estimation (see [baseline_linear()]).}
#'   \item{`rubberband`}{Rubberband baseline estimation (see
#'   [baseline_rubberband()]).}
#'   \item{`SNIP`}{Sensitive Nonlinear Iterative Peak clipping algorithm
#'   (see [baseline_snip()]).}
#'   \item{`4S`}{4S Peak Filling (see [baseline_peakfilling()]).}
#'  }
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-correct.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_correct-method
setGeneric(
  name = "signal_correct",
  def = function(x, y, ...) standardGeneric("signal_correct"),
  valueClass = "list"
)

#' Bind
#'
#' Combines XY objects.
#' @param ... Any object that can be interpreted in a suitable way
#'  (see [grDevices::xy.coords()]).
#' @return
#'  Returns a [`matrix`] of intensities.
#' @author N. Frerebeau
#' @example inst/examples/ex-mean.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_bind-method
setGeneric(
  name = "signal_bind",
  def = function(...) standardGeneric("signal_bind"),
  valueClass = "matrix"
)

#' Mean Intensities
#'
#' @param ... Any object that can be interpreted in a suitable way
#'  (see [grDevices::xy.coords()]).
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-mean.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_mean-method
setGeneric(
  name = "signal_mean",
  def = function(...) standardGeneric("signal_mean"),
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

#' Shift the X Scale
#'
#' Shifts the `x` scale by a given value.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param lag A [`numeric`] vector specifying the offset.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-shift.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_shift-method
setGeneric(
  name = "signal_shift",
  def = function(x, y, lag, ...) standardGeneric("signal_shift"),
  valueClass = "list"
)

#' Drift Intensities
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param lag A [`numeric`] vector specifying the offset or any object that can
#'  be interpreted in a suitable way (see [grDevices::xy.coords()])
#' @param subtract A [`logical`] scalar: should `lag` be subtracted to `y`?
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @author N. Frerebeau
#' @example inst/examples/ex-drift.R
#' @docType methods
#' @family signal processing methods
#' @aliases signal_drift-method
setGeneric(
  name = "signal_drift",
  def = function(x, y, lag, ...) standardGeneric("signal_drift"),
  valueClass = "list"
)

# Smooth =======================================================================
#' Rectangular Smoothing
#'
#' Unweighted sliding-average or rectangular Smoothing.
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used; see [`window_sliding()`]).
#' @param ... Currently not used.
#' @details
#'  It replaces each point in the signal with the average of \eqn{m} adjacent
#'  points.
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  used at both ends of the data series.
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
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used; see [`window_sliding()`]).
#' @param ... Currently not used.
#' @details
#'  It replaces each point in the signal with the weighted mean of \eqn{m}
#'  adjacent points.
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  used at both ends of the data series.
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
#' @param m An odd [`integer`] giving the window size (i.e. the number of
#'  adjacent points to be used).
#' @param p An [`integer`] giving the degree of the polynomial to be used.
#' @param ... Currently not used.
#' @details
#'  This method is based on the least-squares fitting of polynomials to
#'  segments of \eqn{m} adjacent points.
#' @note
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, the original \eqn{(m - 1) / 2} points at
#'  both ends of the data series are preserved.
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

#' Whittaker Smoothing
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param lambda An [`integer`] giving the smoothing parameter. The larger
#'  `lambda` is, the smoother the curve.
#' @param d An [`integer`] specifying the order of the penalty.
#' @param sparse A [`logical`] scalar: should sparse matrices be used for
#'  computation? If `TRUE`, \pkg{Matrix} is required.
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @references
#'  Eilers, P. H. C. (2003). A Perfect Smoother. *Analytical Chemistry*,
#'  75(14): 3631-36. \doi{10.1021/ac034173t}.
#' @author N. Frerebeau
#' @example inst/examples/ex-smooth.R
#' @docType methods
#' @family smoothing methods
#' @aliases smooth_whittaker-method
setGeneric(
  name = "smooth_whittaker",
  def = function(x, y, ...) standardGeneric("smooth_whittaker"),
  valueClass = "list"
)

#' Penalized Likelihood Smoothing
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param lambda An [`integer`] giving the smoothing parameter. The larger
#'  `lambda` is, the smoother the curve.
#' @param d An [`integer`] specifying the order of the penalty.
#' @param SE A [`logical`] scalar: should standard errors be returned?
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @note
#'  \pkg{Matrix} is required.
#' @references
#'  de Rooi, J. J., van der Pers, N. M., Hendrikx, R. W. A., Delhez, R.,
#'  Böttger A. J. and Eilers, P. H. C. (2014). Smoothing of X-ray diffraction
#'  data and Ka2 elimination using penalized likelihood and the composite link
#'  model. *Journal of Applied Crystallography*, 47: 852-860.
#'  \doi{10.1107/S1600576714005809}
#' @author J. J. de Rooi *et al.* (original R code).
#' @example inst/examples/ex-xrd-ka2.R
#' @docType methods
#' @family smoothing methods
#' @aliases smooth_likelihood-method
setGeneric(
  name = "smooth_likelihood",
  def = function(x, y, ...) standardGeneric("smooth_likelihood"),
  valueClass = "list"
)

# Strip ========================================================================
#' Strip XRD ka2
#'
#' @param x,y A [`numeric`] vector. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param lambda An [`integer`] giving the smoothing parameter. The larger
#'  `lambda` is, the smoother the curve.
#' @param wave A length-two [`numeric`] vector giving the characteristic
#'  wavelengths of the anode material (defaults to copper).
#' @param tau A length-one [`numeric`] vector giving the ratio between
#'  \eqn{\alpha}1 and \eqn{\alpha}2 line intensities (defaults to 1/2).
#' @param nseg A length-one [`numeric`] vector specifying the number of equally
#'  sized segments for B-spline basis matrix computation.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] with two components `x` and `y`.
#' @note
#'  \pkg{Matrix} is required.
#' @author J. J. de Rooi *et al.* (original R code).
#' @references
#'  de Rooi, J. J., van der Pers, N. M., Hendrikx, R. W. A., Delhez, R.,
#'  Böttger A. J. and Eilers, P. H. C. (2014). Smoothing of X-ray diffraction
#'  data and Ka2 elimination using penalized likelihood and the composite link
#'  model. *Journal of Applied Crystallography*, 47: 852-860.
#'  \doi{10.1107/S1600576714005809}
#' @example inst/examples/ex-xrd-ka2.R
#' @docType methods
#' @family specialized tools
#' @aliases ka2_strip_penalized-method
setGeneric(
  name = "ka2_strip_penalized",
  def = function(x, y, ...) standardGeneric("ka2_strip_penalized"),
  valueClass = "list"
)

# Windows ======================================================================
#' Sliding Windows
#'
#' @param n An [`integer`] giving the length of the data series (will be coerced
#'  with [`as.integer()`] and hence truncated toward zero).
#' @param m An odd [`integer`] giving the window size, i.e. the number of
#'  adjacent points to be used (will be coerced with [`as.integer()`] and hence
#'  truncated toward zero).
#' @param i A vector [`integer`] specifying the indices of the data points for
#'  which windows should be returned. If `NULL` (the default), windows are
#'  evaluated for each data point.
#' @description
#'  There will be \eqn{(m - 1) / 2} points both at the beginning and at the end
#'  of the data series for which a complete \eqn{m}-width window cannot be
#'  obtained. To prevent data loss, progressively wider/narrower windows are
#'  evaluated at both ends of the data series.
#' @param ... Currently not used.
#' @return
#'  Returns a length-\eqn{n} [`list`] of [`integer`] vectors (indices of the
#'  data points in each window).
#' @author N. Frerebeau
#' @example inst/examples/ex-windows.R
#' @docType methods
#' @family moving windows
#' @aliases window_sliding-method
setGeneric(
  name = "window_sliding",
  def = function(n, m, ...) standardGeneric("window_sliding"),
  valueClass = "list"
)

#' Tumbling Windows
#'
#' @param n An [`integer`] giving the length of the data series (will be coerced
#'  with [`as.integer()`] and hence truncated toward zero).
#' @param m An [`integer`] giving the window size, i.e. the number of
#'  adjacent points to be used (will be coerced with [`as.integer()`] and hence
#'  truncated toward zero).
#' @param drop A [`logical`] scalar: if `m` is not a multiple of `n`, should the
#' last data points be removed so that all windows have the same length?
#' @param ... Currently not used.
#' @return
#'  Returns a [`list`] of [`integer`] vectors (indices of the data points in
#'  each window).
#' @author N. Frerebeau
#' @example inst/examples/ex-windows.R
#' @docType methods
#' @family moving windows
#' @aliases window_tumbling-method
setGeneric(
  name = "window_tumbling",
  def = function(n, m, ...) standardGeneric("window_tumbling"),
  valueClass = "list"
)
