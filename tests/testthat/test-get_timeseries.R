test_that("I can get a timeseries ID", {
  r <- get_timeseries_id(
    parameter_type = "Water Course Discharge",
    station_number = "410730",
    ts_name = "DMQaQc.Merged.DailyMean.24HR"
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(ncol(r), 7)
  expect_equal(nrow(r), 1)
  expect_equal(r$ts_id, "1573010")
})


test_that("I can get timeseries values", {
  # Berthong annual rainfall
  r <- get_timeseries_values(
    ts_id = 148131010,
    start_date = "2016-01-01",
    end_date = "2016-12-31",
    return_fields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(ncol(r), 4)
  expect_equal(nrow(r), 1)
  expect_equal(r$Value, "879")
})

test_that("No results for parameter type and ID mistmatch", {
  r <- get_timeseries_id(
    parameter_type = "Water Course Discharge",
    station_number = "570946",
    ts_name = "DMQaQc.Merged.DailyMean.24HR"
  )

  expect_equal(nrow(r), 0)
  expect_equal(ncol(r), 7)
  expect_equal(class(r), c("tbl_df", "tbl", "data.frame"))
})

test_that("get timeseries puts it all together", {
  r <- get_timeseries(
    parameter_type = "Water Course Discharge",
    station_number = "410730",
    start_date = "2020-01-01",
    end_date = "2020-01-07",
    tz = NULL,
    return_fields = "Timestamp,Value,Quality Code",
    ts_name = "DMQaQc.Merged.DailyMean.24HR"
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(ncol(r), 3)
  expect_equal(nrow(r), 7)
  expect_equal(class(r$Timestamp)[1], "POSIXct")
  expect_true(is.numeric(r$Value))
  expect_true(is.integer(r$`Quality Code`))
})
