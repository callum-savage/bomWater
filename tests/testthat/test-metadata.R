test_that("Internal data is available", {
  expect_snapshot_value(bw_parameters_data, style = "deparse")
  expect_snapshot_value(bw_interpolation_types_data, style = "deparse")
  expect_snapshot_value(bw_quality_codes_data, style = "deparse")
})

test_that("I can get a vector of parameters", {
  all <- bw_parameters()
  disc <- bw_parameters("discrete")
  cont <- bw_parameters("continuous")
  expect_length(all, 14)
  expect_length(cont, 12)
  expect_length(disc, 2)
  expect_equal(all, c(cont, disc))
  expect_true("Ground Water Level" %in% all)
})
