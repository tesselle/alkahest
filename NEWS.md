# alkahest 1.1.1

## Bugfixes & changes
* Follow changes in **Matrix** 1.6-0 (`Matrix::solve(a=<Matrix>, b=<vector>)` returns a vector).

# alkahest 1.1.0
## New classes and methods
* Add penalized likelihood smoothing (de Rooi et al. 2014).
* Add ka2 radiation removal method (de Rooi et al., 2014).

## Bugfixes & changes
* `baseline_peakfilling()` now smoothes the data using the Whittaker smoother as defined in Liland (2015).

## Internals
* Use **tinytest** instead of **testthat**.

# alkahest 1.0.0

* First release
