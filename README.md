
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alkahest

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/alkahest/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/alkahest/actions)
[![codecov](https://codecov.io/gh/tesselle/alkahest/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tesselle/alkahest)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/alkahest/badge/main)](https://www.codefactor.io/repository/github/tesselle/alkahest/overview/main)
[![Dependencies](https://tinyverse.netlify.com/badge/alkahest)](https://cran.r-project.org/package=alkahest)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/alkahest"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=alkahest"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/alkahest"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_alkahest.html"
class="pkgdown-release"><img
src="https://cranchecks.info/badges/worst/alkahest"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=alkahest"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/alkahest"
alt="CRAN Downloads" /></a>

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

**alkahest** is a lightweight, dependency-free toolbox for
pre-processing XY data from experimental methods (i.e. any signal that
can be measured along a continuous variable). It provides methods for
baseline estimation and correction, smoothing, normalization,
integration and peaks detection.

## Installation

You can install the released version of **alkahest** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("alkahest")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/alkahest")
```

## Usage

``` r
## Load the package
library(alkahest)
```

**alkahest** expects the input data to be in the simplest form (a
two-column matrix or data frame, a two-element list or two numeric
vectors).

``` r
## X-ray diffraction
data("XRD")

## Savitzky–Golay filter
smooth <- smooth_savitzky(XRD, m = 11, p = 2)

## 4S Peak Filling baseline
baseline <- baseline_peakfilling(smooth, n = 10, m = 5, by = 10)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")
```

![](man/figures/README-baseline-1.png)<!-- -->

``` r
## Correct baseline
XRD$count <- XRD$count - baseline$y

## Find peaks
peaks <- peaks_find(XRD, SNR = 3, m = 5)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(peaks, type = "p", pch = 16, col = "red")
```

![](man/figures/README-peaks-1.png)<!-- -->

``` r
## Simulate data
set.seed(12345)
x <- seq(-4, 4, length = 100)
y <- dnorm(x)
z <- y + rnorm(100, mean = 0, sd = 0.01) # Add some noise

## Plot raw data
plot(x, z, type = "l", xlab = "", ylab = "", main = "Raw data")
lines(x, y, type = "l", lty = 2, col = "red")

## Whittaker smoothing
smooth <- smooth_whittaker(x, z, lambda = 1000, d = 3)
plot(smooth, type = "l", xlab = "", ylab = "", main = "Whittaker smoothing")
lines(x, y, type = "l", lty = 2, col = "red")
```

<img src="man/figures/README-smooth-1.png" width="50%" /><img src="man/figures/README-smooth-2.png" width="50%" />

## Contributing

Please note that the **alkahest** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
