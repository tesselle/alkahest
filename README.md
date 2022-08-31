
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alkahest

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/alkahest/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/alkahest/actions)
[![codecov](https://codecov.io/gh/tesselle/alkahest/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tesselle/alkahest)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/alkahest/badge/main)](https://www.codefactor.io/repository/github/tesselle/alkahest/overview/main)

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

Pre-Process XY Data from Experimental Methods. **alkahest** is a toolbox
for pre-processing XY data from experimental methods (i.e. any signal
that can be measured along a continuous variable). It provides methods
for baseline correction, smoothing, normalization and integration.

This package depends only on the base R packages.

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
vectors). With few exceptions, all functions return a list with two
elements `x` and `y`.

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
## Correct baseline
BEGe_correct <- signal_correct(BEGe, method = "SNIP")

## Plot spectrum
plot(BEGe_correct, type = "l", xlab = "Energy (keV)", ylab = "Count")

## Find peaks
BEGe_peaks <- peaks_find(BEGe_correct, SNR = 3, m = 5)
lines(BEGe_peaks, type = "p", pch = 16, col = "red")
```

![](man/figures/README-peaks-1.png)<!-- -->

``` r
## gamma-ray spectrometry
data("LaBr")

## Subset from 1350 to 1650 keV
LaBr <- signal_select(LaBr, from = 1350, to = 1650)

## Plot raw spectrum
plot(LaBr, type = "l", xlab = "Energy (keV)", ylab = "Count", main = "Raw data")

## Rectangular smoothing
LaBr_unweighted <- smooth_rectangular(LaBr, m = 3)
plot(LaBr_unweighted, type = "l", xlab = "Energy (keV)", ylab = "Count",
     main = "Rectangular smoothing") 

## Triangular smoothing
LaBr_weighted <- smooth_triangular(LaBr, m = 5)
plot(LaBr_weighted, type = "l", xlab = "Energy (keV)", ylab = "Count",
     main = "Triangular smoothing")

## Savitzky–Golay filter
LaBr_savitzky <- smooth_savitzky(LaBr, m = 21, p = 2)
plot(LaBr_savitzky, type = "l", xlab = "Energy (keV)", ylab = "Count",
     main = "Savitzky–Golay filter")
```

<img src="man/figures/README-smooth-1.png" width="50%" /><img src="man/figures/README-smooth-2.png" width="50%" /><img src="man/figures/README-smooth-3.png" width="50%" /><img src="man/figures/README-smooth-4.png" width="50%" />

## Contributing

Please note that the **alkahest** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.
