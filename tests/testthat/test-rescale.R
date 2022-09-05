test_that("Normalize by AUC", {
  data("XRD")

  XRD_area <- rescale_area(XRD)
  expect_equal(integrate_rectangle(XRD_area), 1)
})
test_that("Normalize by total intensity", {
  data("XRD")

  XRD_total <- rescale_total(XRD, total = 1)
  expect_equal(sum(XRD_total$y), 1)
})
test_that("Normalize max to 1", {
  data("XRD")

  expect_error(rescale_max(XRD, max = 1), "must be lower than")

  XRD_max <- rescale_max(XRD, max = 35000)
  expect_equal(max(XRD_max$y), 35000)
})
test_that("Normalize min to 0", {
  data("XRD")

  XRD_min <- rescale_min(XRD, min = 0)
  expect_equal(min(XRD_min$y), 0)
})
test_that("Normalize 0-1", {
  data("XRD")

  XRD_range <- rescale_range(XRD, min = 0, max = 1)
  expect_equal(min(XRD_range$y), 0)
  expect_equal(max(XRD_range$y), 1)
})
test_that("Transform", {
  data("XRD")

  XRD_trans <- rescale_transform(XRD, f = sqrt)
  expect_equal(max(XRD_trans$y), sqrt(max(XRD$count)))
})
