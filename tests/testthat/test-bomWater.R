#not_cran <- Sys.getenv("NOT_CRAN")
#internet <- curl::has_internet()

test_that("I can make requests to BoM", {
  return_fields <- c("station_name", "station_no", "station_id", "station_latitude", "station_longitude")
  params <- list(
    request = "getStationList",
    parameter_type_name = "Water Course Discharge",
    station_no = "410730",
    returnfields = paste(return_fields, collapse = ",")
  )
  r <- make_bom_request(params)

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(r$station_id, "13360")
  expect_equal(ncol(r), 5)
  expect_equal(nrow(r), 1)
})

test_that("Large requests cause an error", {
  # Request with > 250000 results
  params <- list(
    request = "getTimeseriesValues",
    ts_id = "1571010",
    from = "1900-01-01T00:00:00+10:00",
    to = "2020-01-31T00:00:00+10:00",
    returnfields = "Timestamp,Value,Quality Code,Interpolation Type"
  )
  expect_error(make_bom_request(params))
  # TODO add error message checking
})

test_that("I can get a station list", {
  r <- get_station_list(
    parameter_type = "Rainfall",
    station_number = "570946"
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(nrow(r), 1)
  expect_equal(r$station_name, "Cotter Hut")
  expect_equal(r$station_no, 570946)
  expect_equal(r$station_id, 13643)
  expect_equal(r$station_latitude,-35.64947222)
  expect_equal(r$station_longitude, 148.83144444)
})


test_that("I can get a list of multiple stations", {
  r <- get_station_list(
    parameter_type = "Rainfall",
    station_number = c("570946", "410730")
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(nrow(r), 2)
  expect_equal(r$station_name[2], "Cotter R. at Gingera")
})

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
