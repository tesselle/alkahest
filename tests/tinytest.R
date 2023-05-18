if (requireNamespace("tinytest", quietly = TRUE)) {
  ## Force tests to be executed if in dev release
  ## (which we define as having a sub-release)
  NOT_CRAN <- length(unclass(packageVersion("alkahest"))[[1]]) == 4

  tinytest::test_package("alkahest", at_home = NOT_CRAN)
}
