test_that("I can get a station list", {
  r <- get_station_list(
    parameter_type = "Rainfall",
    station_number = "570946",
    return_fields = c(
      "station_no",
      "station_id",
      "station_name",
      "station_latitude",
      "station_longitude"
    )
  )

  expect_equal(class(r)[1], "tbl_df")
  expect_equal(nrow(r), 1)
  expect_equal(r$station_name, "Cotter Hut")
  expect_equal(r$station_no, "570946")
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
