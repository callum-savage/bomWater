test_that("Requests with no result return an empty tibble", {
  r <- get_monthly(
    parameter = "Evaporation",
    station_number = "410102",
    start_date = "2019-01-01",
    end_date = "2019-12-31",
    return_fields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")
  )

  expect_equal(nrow(r), 0)
  expect_equal(ncol(r), 4)
})
