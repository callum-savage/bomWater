
#' @template timeseriesDocs
#' @examples
#' # Groundwater level as stored by the BoM
#' # PLUMB RD @ NARRABRI'
#' \dontrun{
#' get_as_stored(
#'   parameter = "Ground Water Level",
#'   station_number = "GW971623.3.3",
#'   start_date = "2020-03-01",
#'   end_date = "2020-03-01"
#' )
#' }
#' @export
get_as_stored <- function(parameter,
                          station_number,
                          start_date,
                          end_date,
                          tz = NULL,
                          return_fields = c("Timestamp", "Value", "Quality Code")) {
  parameter <- parameters()[tolower(parameter) == tolower(parameters())]
  if (length(parameter) == 0) {
    stop("Invalid parameter requested")
  }

  timeseries_values <- get_timeseries(
    parameter = parameter,
    station_number = station_number,
    start_date = start_date,
    end_date = end_date,
    tz = tz,
    return_fields = return_fields,
    ts_name = "DMQaQc.Merged.AsStored.1"
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @examples
#' # Hourly streamflow Cotter River at Gingera Gauge
#' \dontrun{
#' get_hourly(
#'   parameter = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31"
#' )
#' }
#' @export
get_hourly <- function(parameter,
                       station_number,
                       start_date,
                       end_date,
                       tz = NULL,
                       return_fields = c("Timestamp", "Value", "Quality Code", "Interpolation Type")) {
  parameter <- parameters()[tolower(parameter) == tolower(parameters())]

  if (!parameter %in% c(
    "Water Course Discharge",
    "Water Course Level",
    "Storage Level",
    "Storage Volume"
  )) {
    stop(
      paste("Hourly data is not available for parameter", parameter)
    )
  }

  timeseries_values <- get_timeseries(
    parameter = parameter,
    station_number = station_number,
    start_date = start_date,
    end_date = end_date,
    tz = tz,
    return_fields = return_fields,
    ts_name = "DMQaQc.Merged.HourlyMean.HR"
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @param var The daily variable of interest. Valid inputs are `mean`, `min`,
#' `max` for continuous series such as discharge and `total` for discrete
#' series such as rainfall and evaporation.
#' @param aggregation Whether the data is to be aggregated midnight to
#' midnight (`24HR`) or from 9am-9am (`09HR`). The default is `24HR`. `09HR`
#' is only available for mean discharge and total rainfall and evaporation.
#' @examples
#' # Download daily mean aggregated over the standard day
#' \dontrun{
#' get_daily(
#'   parameter = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "24HR"
#' )
#' }
#'
#' # Download daily mean aggregated between 9am to 9am
#' \dontrun{
#' get_daily(
#'   parameter = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "mean",
#'   aggregation = "09HR"
#' )
#' }
#'
#' # Download the daily max over the standard day
#' \dontrun{
#' get_daily(
#'   parameter = "Water Course Discharge",
#'   station_number = "410730",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-31",
#'   var = "max",
#'   aggregation = "24HR"
#' )
#' }
#'
#' @export
get_daily <- function(parameter,
                      station_number,
                      start_date,
                      end_date,
                      var = NULL,
                      aggregation = "24HR",
                      tz,
                      return_fields) {
  parameter <-
    parameters()[tolower(parameter) == tolower(parameters())]
  if (length(parameter) == 0) {
    stop("Invalid parameter requested")
  }

  # Handle possible formats of var input
  if (is.null(var)) {
    if (parameter %in% parameters("discrete")) {
      var <- "Total"
    } else {
      var <- "Mean"
    }
  } else {
    var <- stringr::str_to_title(var)
  }
  if (missing(aggregation)) {
    aggregation <- "24HR"
  } else {
    aggregation <- toupper(aggregation)
  }

  ts_name <- paste0("DMQaQc.Merged.Daily", var, ".", aggregation)

  if (parameter %in% parameters("continuous")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyMean.24HR",
      "DMQaQc.Merged.DailyMax.24HR",
      "DMQaQc.Merged.DailyMin.24HR"
    )
    if (parameter == "Water Course Discharge") {
      valid_daily_ts <- c(
        valid_daily_ts,
        "DMQaQc.Merged.DailyMean.09HR"
      )
    }
  }

  if (parameter %in% parameters("discrete")) {
    valid_daily_ts <- c(
      "DMQaQc.Merged.DailyTotal.09HR",
      "DMQaQc.Merged.DailyTotal.24HR"
    )
  }

  if (!ts_name %in% valid_daily_ts) {
    stop("Invalid combination of parameter, var and aggregation")
  }

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @examples
#' # Monthly average dry air temperature at Corin Dam
#' \dontrun{
#' get_monthly(
#'   parameter = "Dry Air Temperature",
#'   station_number = "570947",
#'   start_date = "2016-01-01",
#'   end_date = "2016-06-01"
#' )
#' }
#' @export
get_monthly <- function(parameter,
                        station_number,
                        start_date,
                        end_date,
                        tz,
                        return_fields) {
  parameter <-
    parameters()[tolower(parameter) == tolower(parameters())]
  if (length(parameter) == 0) {
    stop("Invalid parameter requested")
  }

  if (parameter %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.MonthlyMean.CalMonth"
  }

  if (parameter %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.MonthlyTotal.CalMonth")
  }

  if (!exists("ts_name")) stop("Invalid parameter")

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}

#' @template timeseriesDocs
#' @param start_date Start date (formatted as YYYY-MM-DD) or just the
#' year (YYYY)
#' @param end_date End date (formatted as YYYY-MM-DD) or just the year (YYYY)
#' @examples
#' # Download annual rainfall for Cotter Hut
#' \dontrun{
#' get_yearly(
#'   parameter = "Rainfall",
#'   station_number = "570946",
#'   start_date = 2016,
#'   end_date = 2020
#' )
#' }
#'
#' @export
get_yearly <- function(parameter,
                       station_number,
                       start_date,
                       end_date,
                       tz,
                       return_fields) {
  parameter <-
    parameters()[tolower(parameter) == tolower(parameters())]
  if (length(parameter) == 0) {
    stop("Invalid parameter requested")
  }

  start_date <- paste0(stringr::str_sub(start_date, 1, 4), "-01-01")
  end_date <- paste0(stringr::str_sub(end_date, 1, 4), "-12-31")

  if (parameter %in% parameters("continuous")) {
    ts_name <- "DMQaQc.Merged.YearlyMean.CalYear"
  }

  if (parameter %in% parameters("discrete")) {
    ts_name <- c("DMQaQc.Merged.YearlyTotal.CalYear")
  }

  if (!exists("ts_name")) stop("Invalid parameter")

  if (missing(tz)) tz <- NULL

  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_values <- get_timeseries(
    parameter,
    station_number,
    start_date,
    end_date,
    tz,
    return_fields,
    ts_name
  )

  return(timeseries_values)
}
