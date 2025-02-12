# XRD
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ka2_strip_penalized
#' @aliases ka2_strip_penalized,numeric,numeric-method
setMethod(
  f = "ka2_strip_penalized",
  signature = signature(x = "numeric", y = "numeric"),
  definition = function(x, y, lambda, wave = c(1.54060, 1.54443), tau = 0.5,
                        nseg = 1, progress = interactive()) {
    assert_Matrix()
    assert_length(y, length(x))

    ## Calculate doublet distance
    wl1 <- wave[1]
    wl2 <- wave[2]
    wr <- wl2 / wl1
    d2r <- pi / 180
    u <- sin(d2r * x / 2) / wl1
    delta <- 2 * asin(wr * sin(d2r * x / 2)) / d2r - x

    m <- length(y)
    n <- length(lambda)
    if (nseg == 1) nseg <- signif(m * 0.50, 1)

    ## Empty matrices to store the results
    aic <- phi <- psi <- rep(0, n)
    Yhat <- Yhat2 <- matrix(data = 0, nrow = m, ncol = n)
    mu <- matrix(data = 0, nrow = m, ncol = n)
    sr <- matrix(data = 0, nrow = m, ncol = n)

    ## Prepare model matrices
    xr <- max(x)
    xl <- min(x) - 1
    xd <- x - delta
    dx <- (xr - xl) / nseg
    knots <- seq(xl - 3 * dx, xr + 3 * dx, by = dx)

    B1 <- sbase(x, lower = xl, upper = xr, n = nseg, deg = 3)
    B2 <- sbase(xd, lower = xl, upper = xr, n = nseg, deg = 3)

    B <- Matrix::rbind2(B1, B2)
    Cb <- Matrix::Diagonal(m)
    C <- Matrix::cbind2(Cb, tau * Cb)

    ## Prepare penalty
    E <- Matrix::Diagonal(ncol(B1))
    D <- Matrix::diff(E, diff = 2)

    ## Initialize
    bst <- Matrix::solve(
      Matrix::crossprod(B1, B1) + 1e-4 * E,
      Matrix::crossprod(B1, log((y + 1) / 2))
    )

    if (progress) pb <- utils::txtProgressBar(min = 0, max = n, style = 3)

    ## Estimate for all lambda
    n_lambda <- seq_len(n)
    for (i in n_lambda) {
      M <- penalized_strip_ka2(y, B, C, D, bst, lambda[i])
      aic[i] <- M$aic
      Yhat[, i] <- M$yhat
      Yhat2[, i] <- M$yhat2
      sr[,i] <- M$res
      mu[, i] <- M$mu
      phi[i] <- M$phi
      psi[i] <- M$psi
      if (progress) utils::setTxtProgressBar(pb, i)
    }

    if (progress) close(pb)

    ## Find optimal model and report
    k <- which.min(aic)
    mu <- mu[, k]
    op_lamb <- lambda[k]
    op_aic <- aic[k]
    yhat <- Yhat[, k]
    yhat2 <- Yhat2[, k]
    sr <- sr[, k]

    xy <- list(x = x, y = yhat, ka2 = yhat2, smoothed = mu)
    xy
  }
)

#' @export
#' @rdname ka2_strip_penalized
#' @aliases ka2_strip_penalized,ANY,missing-method
setMethod(
  f = "ka2_strip_penalized",
  signature = signature(x = "ANY", y = "missing"),
  definition = function(x, lambda, wave = c(1.54060, 1.54443), tau = 0.5,
                        nseg = 1, progress = interactive()) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y, lambda = lambda, wave = wave,
                         tau = tau, nseg = nseg, progress = progress)
  }
)


#' Truncated p-th Power
#'
#' @param x A [`numeric`] vector on which the basis is calculated.
#' @param knot A [`numeric`] scalar giving the truncation point.
#' @param p A [`numeric`] scalar specifying the power for the basis
#'  (e.g. \eqn{p = 3} for cubic TPF).
#' @return
#'  A [`numeric`] vector (piece-wise defined basis functions for \eqn{x > t}).
#' @author P. Eilers
#' @note
#'  Slightly modified from P. Eilers' [JOPS::tpower()].
#' @references
#'  Eilers, P. H. C. and Marx, B. D. (2021). *Practical Smoothing, The Joys of
#'  P-splines.* Cambridge: Cambridge University Press.
#' @keywords internal
#' @noRd
tpower <- function(x, knot, p) {
  (x - knot) ^ p * (x > knot)
}

#' B-spline Design Matrix
#'
#' Computes a B-spline design matrix using evenly spaced knots.
#' @param x A [`numeric`] vector of values at which the B-spline basis functions
#'  are to be evaluated.
#' @param lower A [`numeric`] vector giving the lower limit of the domain of `x`.
#' @param upper A [`numeric`] vector giving the upper limit of the domain of `x`.
#' @param n A [`numeric`] scalar specifying the number of equally sized segments
#'  between `lower` and `upper`.
#' @param deg A [`numeric`] scalar specifyingthe degree of the splines,
#'  usually 1, 2, or 3 (the default).
#' @return
#'  A `matrix` with  `length(x)` rows and `n + deg` columns.
#' @note
#'  Slightly modified from P. Eilers and B. Marx's [JOPS::bbase()].
#' @author P. Eilers, B. Marx
#' @references
#'  Eilers, P. H. C. and Marx, B. D. (2021). *Practical Smoothing, The Joys of
#'  P-splines.* Cambridge: Cambridge University Press.
#' @keywords internal
#' @noRd
bbase <- function(x, lower = min(x), upper = max(x), n = 10, deg = 3) {
  ## Check domain and adjust it if necessary
  if (lower > min(x)) {
    lower <- min(x)
    warning(sprintf(tr_("Left boundary adjusted to %g."), lower), call. = FALSE)
  }
  if (upper <  max(x)) {
    upper <- max(x)
    warning(sprintf(tr_("Right boundary adjusted to %g."), upper), call. = FALSE)
  }

  ## Function for B-spline basis
  dx <- (upper - lower) / n
  knots <- seq(lower - deg * dx, upper + deg * dx, by = dx)
  P <- outer(x, knots, tpower, deg)
  n <- dim(P)[[2L]]
  D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
  B <- (-1) ^ (deg + 1) * P %*% t(D)

  ## Make B-splines exactly zero beyond their end knots
  nb <- ncol(B)
  sk <- knots[(1:nb) + deg + 1]
  mask <- outer(x, sk, '<')
  B <- B * mask

  B
}

#' Sparse B-spline Basis Matrix
#'
#' Computes a B-spline basis matrix using evenly spaced knots.
#' @param x A [`numeric`] vector of values at which the B-spline basis functions
#'  are to be evaluated.
#' @param lower A [`numeric`] vector giving the lower limit of the domain of `x`.
#' @param upper A [`numeric`] vector giving the upper limit of the domain of `x`.
#' @param n A [`numeric`] scalar specifying the number of equally sized segments
#'  between `lower` and `upper`.
#' @param deg A [`numeric`] scalar specifyingthe degree of the splines,
#'  usually 1, 2, or 3 (the default).
#' @return
#'  A [sparse matrix][Matrix::sparseMatrix()] with `length(x)` rows and
#'  `n + deg` columns.
#' @note
#'  Slightly modified from P. Eilers [JOPS::spbase()].
#' @author P. Eilers, B. Marx
#' @references
#'  Eilers, P. H. C. and Marx, B. D. (2021). *Practical Smoothing, The Joys of
#'  P-splines.* Cambridge: Cambridge University Press.
#' @keywords internal
#' @noRd
sbase <- function(x, lower = min(x), upper = max(x), n = 10, deg = 3) {
  ## Check domain and adjust it if necessary
  if (lower > min(x)) {
    lower <- min(x)
    warning(sprintf(tr_("Left boundary adjusted to %g."), lower), call. = FALSE)
  }
  if (upper <  max(x)) {
    upper <- max(x)
    warning(sprintf(tr_("Right boundary adjusted to %g."), upper), call. = FALSE)
  }

  ## Reduce x to first interval between knots
  dx <- (upper - lower) / n
  ix <- floor((x - lower) / (1.0000001 * dx))
  xx <- (x - lower) - ix * dx

  ## Full basis for reduced x
  Br <- bbase(xx, lower = 0, upper = 0 + dx, n = 1, deg = deg)

  ## Compute proper rows, columns
  m <- length(x)
  p <- ncol(Br)
  i_row <- rep(seq_len(m), each = p)
  i_col <- rep(seq_len(p), times = m) + rep(ix, each = p)

  ## Sparse matrix
  b <- as.vector(t(Br))
  Bs <- Matrix::sparseMatrix(i = i_row, j = i_col, x = b, dims = c(m, n + deg))
  Bs <- Bs[, -ncol(Bs), drop = FALSE] # ???

  Bs
}

#' @param y description
#' @param B description
#' @param C description
#' @param D description
#' @param beta description
#' @param lambda description
#' @author J. J. de Rooi *et al.* (original R code).
#' @references
#'  de Rooi, J. J., van der Pers, N. M., Hendrikx, R. W. A., Delhez, R.,
#'  Böttger A. J. and Eilers, P. H. C. (2014). Smoothing of X-ray diffraction
#'  data and Ka2 elimination using penalized likelihood and the composite link
#'  model. *Journal of Applied Crystallography*, 47: 852-860.
#'  \doi{10.1107/S1600576714005809}
#' @keywords internal
#' @noRd
penalized_strip_ka2 <- function(y, B, C, D, beta, lambda) {
  m <- length(y)

  ## Penalty
  P <- lambda * Matrix::t(D) %*% D

  ## Fit iteratively
  delta_beta <- 1
  limit <- 1 # Stop condition to prevent infinite loop
  while (delta_beta >= 0.0001 & limit <= 30) {
    eta <- B %*% beta
    gam <- exp(eta)
    mu <- as.numeric(C %*% gam)

    M <- Matrix::Diagonal(x = 1 / mu)
    X <- (M %*% C %*% Matrix::Diagonal(x = as.numeric(gam))) %*% B
    W <- Matrix::Diagonal(x = mu)
    G <- Matrix::crossprod(X, W) %*% X

    new_beta <- Matrix::solve(G + P, Matrix::crossprod(X, (y - mu)) + G %*% beta)
    delta_beta <- max(abs(new_beta - beta))
    beta <- new_beta

    limit <- limit + 1
  }

  ## AIC
  H <- Matrix::solve(G + P, G)
  ed <- sum(Matrix::diag(H))
  dev <- 2 * sum(y * log((y + (y == 0)) / mu))
  aic <- dev + 2 * ed

  ## Standardized residuals for counts
  sr <- (y - mu) / sqrt(mu)
  sdr <- sqrt(sum(sr^2) / (m - ed))

  ## L-curve
  phi <- log(sum((D %*% beta)^2))
  psi <- log(sum(sr^2))

  ## estimated signals
  yhat <- gam[1:m]
  yhat2 <- gam[(m + 1):length(gam)] * 0.5

  ## return values
  list(yhat = yhat, yhat2 = yhat2, mu = mu, aic = aic, res = sr,
       beta = beta, phi = phi, psi = psi)
}
