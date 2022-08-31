# FIND PEAKS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname peaks_find
#' @aliases peaks_find,numeric,numeric-method
setMethod(
  f = "peaks_find",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, method = c("MAD"), SNR = 2, m = NULL, ...) {
    ## Validation
    method <- match.arg(method, several.ok = FALSE)
    if (is.null(m)) m <- round(length(x) * 0.05)
    m <- as.integer(m)

    ## Noise threshold
    if (SNR != 0) {
      noise <- switch (
        method,
        MAD = stats::mad
      )
      threshold <- noise(y, ...) * SNR
      index_noise <- y < threshold
      y[index_noise] <- 0
    }

    ## Peaks detection
    shape <- diff(sign(diff(y, na.pad = FALSE)))
    index_shape <- lapply(
      X = which(shape < 0),
      FUN = function(i, data, m) {
        n <- length(data)
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < n, w, n)
        if (all(data[c(z:i, (i + 2):w)] <= data[i + 1])) return(i + 1)
        return(NULL)
      },
      data = y,
      m = m
    )
    k <- unlist(index_shape)

    xy <- list(x = x[k], y = y[k])
    attr(xy, "method") <- method
    xy
  }
)

#' @export
#' @rdname peaks_find
#' @aliases peaks_find,ANY,missing-method
setMethod(
  f = "peaks_find",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, method = c("MAD"), SNR = 2, m = NULL, ...) {
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
