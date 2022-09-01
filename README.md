
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alkahest

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/alkahest/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/alkahest/actions)
[![codecov](https://codecov.io/gh/tesselle/alkahest/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tesselle/alkahest)
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
## gamma-ray spectrometry
data("BEGe")

## Subset from 3 to 200 keV
BEGe <- signal_select(BEGe, from = 3, to = 200)

## SNIP baseline
BEGe_snip <- baseline_snip(BEGe, LLS = FALSE, decreasing = FALSE, n = 100)

plot(BEGe, type = "l", xlab = "Energy (keV)", ylab = "Count")
lines(BEGe_snip, type = "l", col = "red")
```

![](man/figures/README-baseline-1.png)<!-- -->

``` r
## X-ray diffraction
data("XRD")

## Correct baseline
XRD_correct <- signal_correct(XRD, method = "SNIP")

## Find peaks
XRD_peaks <- peaks_find(XRD_correct, SNR = 3, m = 5)

plot(XRD_correct, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(XRD_peaks, type = "p", pch = 16, col = "red")
```

![](man/figures/README-peaks-1.png)<!-- -->

``` r
set.seed(12345)
x <- seq(-4, 4, length = 100)
y <- dnorm(x) + rnorm(100, mean = 0, sd = 0.01)

## Plot raw data
plot(x, y, type = "l", xlab = "", ylab = "", main = "Raw data")

## Rectangular smoothing
unweighted <- smooth_rectangular(x, y, m = 3)
plot(unweighted, type = "l", xlab = "", ylab = "", main = "Rectangular smoothing") 

## Triangular smoothing
weighted <- smooth_triangular(x, y, m = 5)
plot(weighted, type = "l", xlab = "", ylab = "", main = "Triangular smoothing")

## Savitzky–Golay filter
savitzky <- smooth_savitzky(x, y, m = 21, p = 2)
plot(savitzky, type = "l", xlab = "", ylab = "", main = "Savitzky–Golay filter")
```

<img src="man/figures/README-smooth-1.png" width="50%" /><img src="man/figures/README-smooth-2.png" width="50%" /><img src="man/figures/README-smooth-3.png" width="50%" /><img src="man/figures/README-smooth-4.png" width="50%" />

## Contributing

Please note that the **alkahest** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
