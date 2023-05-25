#' @title Get time series
#' @md
#' @description Get timeseries data from Water Data online
#' @details This function can be used if you want to retrieve a specific
#' timeseries that is not the default quality checked one.
#'
#' Common valid return fields are:
#'
#' * Timestamp
#' * Value
#' * Quality Code
#' * Interpolation Type
#'
#' Other valid return fields (depending on the parameter requested) may be:
#'
#' * Absolute Value
#' * AV Interpolation
#' * AV Quality Code
#' * Runoff Value
#' * RV Interpolation
#' * RV Quality Code
#' * Aggregation
#' * Accuracy
#'
#' If the request is not valid it will fail.
#' @param parameter The water data parameter type (e.g. Water Course
#' Discharge). See \code{\link{parameters()}} for a full list.
#' @param station_number The AWRC station number.
#' @param start_date Start date formatted as a string or date class
#' (YYYY-MM-DD).
#' @param end_date End date formatted as a string or date class (YYYY-MM-DD).
#' @param tz Optional: the desired time zone for the output timeseries. Input
#' must be an Olson Name (see `OlsonNames()`). By default the timeseries are
#' returned in non-DST time zones (AEST, ACST or AWST) depending on the
#' station location.
#' @param return_fields Optional: columns to be returned from Water Data Online.
#' By default Timestamp, Value and Quality Code are returned.
#' @param ts_name The timeseries name (e.g. DMQaQc.Merged.DailyMean.24HR) that
#' is desired.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
#' @seealso
#' * \url{http://www.bom.gov.au/waterdata/}
#' * \href{http://www.bom.gov.au/waterdata/wiski-web-public/Guide%20to%20Sensor%20Observation%20Services%20(SOS2)%20for%20Water%20Data%20%20Online%20v1.0.1.pdf}{BoM Guide to Sensor Observation Services (SOS2) for Water Data Online}
#' @examples
#' # Accessible dam storage, as shown on the BoM Water Storage dashboard
#' \dontrun{
#' get_timeseries(
#'   parameter = "Storage Volume",
#'   "G8150011",
#'   "2020-01-01",
#'   "2020-01-31",
#'   ts_name = "PR02AVQaQc.Merged.DailyMean.24HR",
#'   tz = NULL,
#'   return_fields = c("Timestamp", "Value", "Quality Code")
#' )
#' }
#' # See the linked SOS2 manual in See Also to find more timeseries names
#' @export
get_timeseries <- function(parameter,
                           station_number,
                           start_date,
                           end_date,
                           tz = NULL,
                           return_fields = c("Timestamp", "Value", "Quality Code"),
                           ts_name) {
  if (is.null(tz)) {
    tz <- get_timezone(parameter, station_number, default = "UTC")
  } else if (!(tz %in% OlsonNames())) {
    stop("Invalid tz argument. Check it is in OlsonNames().")
  }

  start_date <- parse_bom_date(start_date, tz)
  end_date <- parse_bom_date(end_date, tz)
  if (start_date > end_date) {
    stop("start_date must be prior to the end_date")
  }

  if (length(station_number) > 1) {
    stop("Only one station can be requested at a time")
  }

  timeseries_id <- get_timeseries_id(
    parameter = parameter,
    station_number = station_number,
    ts_name = ts_name
  )

  timeseries_values <- get_timeseries_values(
    ts_id = timeseries_id$ts_id[1],
    start_date = start_date,
    end_date = end_date,
    return_fields = return_fields
  )

  # Only process data if it exists
  if (nrow(timeseries_values) > 0) {
    if ("Timestamp" %in% colnames(timeseries_values)) {
      timeseries_values$Timestamp <-
        lubridate::as_datetime(timeseries_values$Timestamp, tz = tz)
    }
    timeseries_values <-
      convert_types(
        timeseries_values,
        dont_convert = c("station_no", "Timestamp")
      )
  }
  return(timeseries_values)
}




#' @title Retrieve the timeseries ID
#' @description
#' `get_timeseries_id` retrieves the timeseries ID that can be used to obtain
#' values for a parameter type, station and timeseries combination.
#' @param parameter The parameter of interest (e.g. Water Course
#' Discharge).
#' @param station_number The AWRC station number.
#' @param ts_name The BoM time series name (e.g. DMQaQc.Merged.DailyMean.24HR).
#' @return
#' Returns a tibble with columns station_name, station_no, station_id, ts_id,
#' ts_name, parametertype_id, parametertype_name.
get_timeseries_id <- function(parameter,
                              station_number,
                              ts_name
) {
  params <- collapse_params(
    request = "getTimeseriesList",
    parametertype_name = parameter,
    ts_name = ts_name,
    station_no = station_number
  )
  make_bom_request(params)
}

#' @title Retrieve timeseries values
#' @description
#' `get_timeseries_values` returns the timeseries values between a start and end
#' date for given timeseries ID.
#' @param ts_id The timeseries ID for the values of interest. Can be found using
#' the function `get_timeseries_id`.
#' @param start_date The start date formatted as 'YYYY-MM-DD'.
#' @param end_date The end date formatted as 'YYYY-MM-DD'.
#' @param return_fields A vector of the variables that are to be returned.
#' @return
#' A tibble with columns with the requested return_fields. A zero row tibble is
#' returned if no data is returned  from the query. The columns of the tibble
#' are returned as character classes and have not been formatted to more
#' appropriate correct classes (this happens in other functions).
get_timeseries_values <- function(ts_id,
                                  start_date,
                                  end_date,
                                  return_fields
) {
  params <- collapse_params(
    request = "getTimeseriesValues",
    ts_id = ts_id,
    from = start_date,
    to = end_date,
    returnfields = paste(return_fields, collapse = ",")
  )
  make_bom_request(params)
}

get_timezone <- function(parameter, station_number, default = "UTC") {
  data_owner <- get_data_owner(parameter, station_number)
  state <- stringr::str_split_fixed(data_owner, " -", n = 2)[1]
  if (state %in% c("ACT", "ACTNSW", "NSW", "QLD", "TAS", "VIC")) {
    tz <- "Australia/Queensland" # AEST
  } else if (state %in% c("SA", "NT")) {
    tz <- "Australia/Darwin" # ACST
  } else if (state == "WA") {
    tz <- "Australia/Perth" # AWST
  } else {
    message("Unable to fine gague timezone, returning default")
    tz <- as.character(default)
  }
  tz
}

parse_bom_date <- function(d, tz) {
  if (is.character(d)) {
    d <- lubridate::as_date(d, format = "%Y-%m-%d")
  }
  if (is.na(d)) {
    stop("Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)")
  }
  if (lubridate::is.Date(d)) {
    d <- lubridate::force_tz(lubridate::as_datetime(d), tz = tz)
  } else if (!lubridate::is.POSIXt(d)) {
    stop("Provide dates as a character, date, or datetime object")
  }
  # Fix the offset because BoM expects 00:00 style
  # despite 0000 conforming to ISO8601
  d <- sub(
    "(.*\\+)(\\d{2})(\\d{2})",
    "\\1\\2:\\3",
    lubridate::format_ISO8601(d, usetz = TRUE)
  )
  d
}
