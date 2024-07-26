
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alkahest <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/alkahest/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/alkahest/actions)
[![codecov](https://codecov.io/gh/tesselle/alkahest/branch/main/graph/badge.svg?token=wsIkAQ0XFK)](https://app.codecov.io/gh/tesselle/alkahest)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/alkahest/badge/main)](https://www.codefactor.io/repository/github/tesselle/alkahest/overview/main)
[![Dependencies](https://tinyverse.netlify.app/badge/alkahest)](https://cran.r-project.org/package=alkahest)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/alkahest"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=alkahest"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/alkahest"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_alkahest.html"
class="pkgdown-release"><img
src="https://badges.cranchecks.info/worst/alkahest.svg"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=alkahest"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/alkahest"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7081524.svg)](https://doi.org/10.5281/zenodo.7081524)
<!-- badges: end -->

## Overview

**alkahest** is a lightweight, dependency-free toolbox for
pre-processing XY data from experimental methods (i.e. any signal that
can be measured along a continuous variable). It provides methods for
baseline estimation and correction, smoothing, normalization,
integration and peaks detection.

- Baseline estimation methods: Linear, Polynomial (Lieber and
  Mahadevan-Jansen 2003), Asymmetric Least Squares (Eilers and Boelens
  2005), Rolling Ball (Kneen and Annegarn 1996), Rubberband, SNIP
  (Morháč et al. 1997; Morháč and Matoušek 2008; Ryan et al. 1988), 4S
  Peak Filling (Liland 2015).
- Smoothing methods: Rectangular, Triangular, Loess, Savitzky-Golay
  Filter (Gorry 1990; Savitzky and Golay 1964), Whittaker (Eilers 2003),
  Penalized Likelihood (De Rooi et al. 2014)

------------------------------------------------------------------------

To cite alkahest in publications use:

Frerebeau N (2024). *alkahest: Pre-Processing XY Data from Experimental
Methods*. Université Bordeaux Montaigne, Pessac, France.
<doi:10.5281/zenodo.7081524> <https://doi.org/10.5281/zenodo.7081524>, R
package version 1.2.0, <https://packages.tesselle.org/alkahest/>.

This package is a part of the tesselle project
<https://www.tesselle.org>.

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

## 4S Peak Filling baseline
baseline <- baseline_peakfilling(XRD, n = 10, m = 5, by = 10, sparse = TRUE)

plot(XRD, type = "l", xlab = expression(2*theta), ylab = "Count")
lines(baseline, type = "l", col = "red")
```

![](man/figures/README-baseline-1.png)<!-- -->

``` r
## Correct baseline
XRD <- signal_drift(XRD, lag = baseline, subtract = TRUE)

## Find peaks
peaks <- peaks_find(XRD, SNR = 3, m = 11)

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

## Savitzky–Golay filter
smooth <- smooth_savitzky(x, z, m = 21, p = 2)
plot(smooth, type = "l", xlab = "", ylab = "", main = "Savitzky–Golay filter")
lines(x, y, type = "l", lty = 2, col = "red")
```

<img src="man/figures/README-smooth-1.png" width="50%" /><img src="man/figures/README-smooth-2.png" width="50%" />

## Contributing

Please note that the **alkahest** project is released with a
[Contributor Code of Conduct](https://www.tesselle.org/conduct.html). By
contributing to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-derooi2014" class="csl-entry">

De Rooi, Johan J., Niek M. Van Der Pers, Ruud W. A. Hendrikx, Rob
Delhez, Amarante J. Böttger, and Paul H. C. Eilers. 2014. “Smoothing of
<span class="nocase">X-ray</span> Diffraction Data and *K* α
<sub>2</sub> Elimination Using Penalized Likelihood and the Composite
Link Model.” *Journal of Applied Crystallography* 47 (3): 852–60.
<https://doi.org/10.1107/S1600576714005809>.

</div>

<div id="ref-eilers2003" class="csl-entry">

Eilers, Paul H. C. 2003. “A Perfect Smoother.” *Analytical Chemistry* 75
(14): 3631–36. <https://doi.org/10.1021/ac034173t>.

</div>

<div id="ref-eilers2005" class="csl-entry">

Eilers, Paul H. C., and Hans F. M. Boelens. 2005. “Baseline Correction
with Asymmetric Least Squares Smoothing.” October 21, 2005.

</div>

<div id="ref-gorry1990" class="csl-entry">

Gorry, Peter A. 1990. “General Least-Squares Smoothing and
Differentiation by the Convolution (Savitzky-Golay) Method.” *Analytical
Chemistry* 62 (6): 570–73. <https://doi.org/10.1021/ac00205a007>.

</div>

<div id="ref-kneen1996" class="csl-entry">

Kneen, M. A., and H. J. Annegarn. 1996. “Algorithm for Fitting XRF, SEM
and <span class="nocase">PIXE X-ray</span> Spectra Backgrounds.”
*Nuclear Instruments and Methods in Physics Research Section B: Beam
Interactions with Materials and Atoms* 109–110 (April): 209–13.
<https://doi.org/10.1016/0168-583X(95)00908-6>.

</div>

<div id="ref-lieber2003" class="csl-entry">

Lieber, Chad A., and Anita Mahadevan-Jansen. 2003. “Automated Method for
Subtraction of Fluorescence from Biological Raman Spectra.” *Applied
Spectroscopy* 57 (11): 1363–67.
<https://doi.org/10.1366/000370203322554518>.

</div>

<div id="ref-liland2015" class="csl-entry">

Liland, Kristian Hovde. 2015. “4S Peak Filling – Baseline Estimation by
Iterative Mean Suppression.” *MethodsX* 2: 135–40.
<https://doi.org/10.1016/j.mex.2015.02.009>.

</div>

<div id="ref-morhac1997" class="csl-entry">

Morháč, Miroslav, Ján Kliman, Vladislav Matoušek, Martin Veselský, and
Ivan Turzo. 1997. “Background Elimination Methods for Multidimensional
Coincidence γ-Ray Spectra.” *Nuclear Instruments and Methods in Physics
Research Section A: Accelerators, Spectrometers, Detectors and
Associated Equipment* 401 (1): 113–32.
<https://doi.org/10.1016/S0168-9002(97)01023-1>.

</div>

<div id="ref-morhac2008" class="csl-entry">

Morháč, Miroslav, and Vladislav Matoušek. 2008. “Peak Clipping
Algorithms for Background Estimation in Spectroscopic Data.” *Applied
Spectroscopy* 62 (1): 91–106.
<https://doi.org/10.1366/000370208783412762>.

</div>

<div id="ref-ryan1988" class="csl-entry">

Ryan, C. G., E. Clayton, W. L. Griffin, S. H. Sie, and D. R. Cousens.
1988. “SNIP, a Statistics-Sensitive Background Treatment for the
Quantitative Analysis of PIXE Spectra in Geoscience Applications.”
*Nuclear Instruments and Methods in Physics Research Section B: Beam
Interactions with Materials and Atoms* 34 (3): 396–402.
<https://doi.org/10.1016/0168-583X(88)90063-8>.

</div>

<div id="ref-savitzky1964" class="csl-entry">

Savitzky, Abraham., and M. J. E. Golay. 1964. “Smoothing and
Differentiation of Data by Simplified Least Squares Procedures.”
*Analytical Chemistry* 36 (8): 1627–39.
<https://doi.org/10.1021/ac60214a047>.

</div>

</div>
