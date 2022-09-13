# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname peaks_find
#' @aliases peaks_find,numeric,numeric-method
setMethod(
  f = "peaks_find",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, method = "MAD", SNR = 2, m = NULL, ...) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)
    if (is.null(m)) {
      m <- as.integer(length(x) * 0.05)
      if (m %% 2 == 0) m <- m + 1
    }

    ## Noise threshold
    threshold <- NULL
    if (SNR != 0) {
      noise <- switch (
        method,
        MAD = stats::mad
      )
      threshold <- noise(y, ...) * SNR
      index_noise <- y < threshold
      y[index_noise] <- 0
    }

    ## Windows
    shape <- diff(sign(diff(y, na.pad = FALSE)))
    win <- window_sliding(length(x), m, i = which(shape < 0) + 1L)

    ## Peaks detection
    pks <- vapply(
      X = win,
      FUN = function(w, k, data) {
        i <- length(w) - k # Middle of the window
        p <- if (all(data[w[-i]] <= data[w[i]])) w[i] else 0
        return(p)
      },
      FUN.VALUE = numeric(1),
      k = (m - 1) / 2,
      data = y
    )

    xy <- list(x = x[pks], y = y[pks])
    attr(xy, "noise") <- threshold
    xy
  }
)

#' @export
#' @rdname peaks_find
#' @aliases peaks_find,ANY,missing-method
setMethod(
  f = "peaks_find",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, method = "MAD", SNR = 2, m = NULL, ...) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, method = method, SNR = SNR,
                         m = m, ...)
  }
)

#' @export
#' @rdname peaks_fwhm
#' @aliases peaks_fwhm,numeric,numeric-method
setMethod(
  f = "peaks_fwhm",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, center) {
    i <- which_nearest(x, center)
    peak_height <- y[i]
    half_max <- peak_height / 2

    scale_for_roots <- y - half_max
    root_indices <- which(diff(sign(scale_for_roots)) != 0)

    tmp <- sort(c(root_indices, i))
    k <- which(tmp == i)

    root_left <- root_indices[k - 1]
    root_right <- root_indices[k]

    HWHM_left <- x[i] - x[root_left]
    HWHM_right <- x[root_right] - x[i]

    # xy <- list(x = c(x[root_left], x[root_right]), y = c(half_max, half_max))

    FWHM <- 2 * min(c(HWHM_left, HWHM_right))
    return(FWHM)
  }
)

#' @export
#' @rdname peaks_fwhm
#' @aliases peaks_fwhm,ANY,missing-method
setMethod(
  f = "peaks_fwhm",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, center) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, center = center)
  }
)
