#not_cran <- Sys.getenv("NOT_CRAN")
#internet <- curl::has_internet()
#
# TESTS TO ADD
#
# - Warn when silently returning no results
# - Check bad date inputs
# - Invalid input types
# - Inputs out of range
# - Unknown return fields or parameters
# - Unknon request (should still return json as not all requests are covered)

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

test_that("Empty responses return an empty tibble", {
  params <- list(
      request = "getTimeseriesValues",
      ts_id = "196412010",
      from = "2019-01-01T00:00:00+00:00",
      to = "2019-12-31T00:00:00+00:00",
      returnfields = "Timestamp,Value,Quality Code"
    )
  r <- make_bom_request(params)
  expect_equal(nrow(r), 0)
  expect_equal(ncol(r), 3)
})
