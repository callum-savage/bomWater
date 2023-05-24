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
#' @param parameter_type The water data parameter type (e.g. Water Course
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
#'   parameter_type = "Storage Volume",
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

get_timeseries <- function(parameter_type,
                           station_number,
                           start_date,
                           end_date,
                           tz = "UTC",
                           return_fields,
                           ts_name) {

  # If no tz supplied, get from jurisdiction
  # All of this is currently skipped by the default field
  if (is.null(tz)) {
    # From BoM website:
    # Which time zones are the data displayed in?
    #   Time of day is presented in local standard time. Coordinated Universal Timezones (UTC) are:
    #
    #   Eastern States (QLD, NSW, ACT, VIC, TAS) - UTC +10:00
    #   Central States (NT, SA) - UTC +09:30
    #   Western Australia - UTC +08:00.

    # Get the station list and custom attributes to determine correct time zone
    station_list <- get_station_list(
      parameter_type = parameter_type,
      station_number = station_number,
      return_fields = "custom_attributes"
    )
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }

    jurisdiction <- stringr::str_split_fixed(station_list$DATA_OWNER_NAME, " -", n = 2)[1]

    # Time zones are selected where there is no DST
    if (jurisdiction %in% c("ACT", "ACTNSW", "NSW", "QLD", "TAS", "VIC")) {
      tz <- "Australia/Queensland" # AEST
    } else if (jurisdiction %in% c("SA", "NT")) {
      tz <- "Australia/Darwin" # ACST
    } else if (jurisdiction == "WA") {
      tz <- "Australia/Perth" # AWST
    } else {
      message("Jurisdiction not found, returning datetimes in UTC")
      tz <- "UTC"
    }
  } else {
    # Check if tz is valid
    if (!tz %in% OlsonNames()) {
      stop("Invalid tz argument. Check it is in OlsonNames().")
    }
    station_list <- get_station_list(parameter_type, station_number)
    if (nrow(station_list) == 0) {
      stop(paste("Station number", station_number, "is invalid"))
    }
  }

  # Check string date input is valid
  if (is.character(start_date)) {
    start_date <- lubridate::as_date(start_date, format = "%Y-%m-%d")
  }
  if (is.character(end_date)) {
    end_date <- lubridate::as_date(end_date, format = "%Y-%m-%d")
  }
  if (anyNA(c(start_date, end_date))) {
    stop("Dates must be formatted as %Y-%m-%d (e.g. 2000-01-01)")
  }
  # Ensure start is less than end
  if (start_date > end_date) {
    stop("start_date must be less than end_date")
  }

  # Coerce to datetime
  if (lubridate::is.Date(start_date)) {
    start_date <- lubridate::force_tz(lubridate::as_datetime(start_date), tz = tz)
    end_date <- lubridate::force_tz(lubridate::as_datetime(end_date), tz = tz)
  } else if (!lubridate::is.POSIXt(start_date)) {
    stop("Provide dates as a character, date, or datetime object")
  }

  # Fix the offset because BoM expects 00:00 style
  # despite 0000 conforming to ISO8601
  start_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(start_date, usetz = TRUE)
  )
  end_date <- sub(
    "(.*\\+)(\\d{2})(\\d{2})", "\\1\\2:\\3",
    lubridate::format_ISO8601(end_date, usetz = TRUE)
  )

  # Only accept one station at a time for now
  if (length(station_number) > 1) {
    stop("Only a single station can be requested at a time")
  }

  # If return_fields are missing return Timestamp, Value and Quality Code
  if (missing(return_fields)) {
    return_fields <- c("Timestamp", "Value", "Quality Code")
  }

  timeseries_id <- get_timeseries_id(
    parameter_type = parameter_type,
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
      # nolint start
      suppressMessages({
        timeseries_values$Timestamp <- lubridate::as_datetime(timeseries_values$Timestamp, tz = tz)
      })
      timeseries_values <- dplyr::mutate_at(timeseries_values,
                                            dplyr::vars(-"Timestamp"),
                                            utils::type.convert,
                                            as.is = TRUE
      )
      # nolint end
    } else {
      timeseries_values <- dplyr::mutate_all(timeseries_values,
                                             utils::type.convert,
                                             as.is = TRUE
      )
    }
  }
  return(timeseries_values)
}




#' @title Retrieve the timeseries ID
#' @description
#' `get_timeseries_id` retrieves the timeseries ID that can be used to obtain
#' values for a parameter type, station and timeseries combination.
#' @param parameter_type The parameter of interest (e.g. Water Course
#' Discharge).
#' @param station_number The AWRC station number.
#' @param ts_name The BoM time series name (e.g. DMQaQc.Merged.DailyMean.24HR).
#' @return
#' Returns a tibble with columns station_name, station_no, station_id, ts_id,
#' ts_name, parametertype_id, parametertype_name.
get_timeseries_id <- function(parameter_type,
                              station_number,
                              ts_name
) {
  params <- list(
    "request" = "getTimeseriesList",
    "parametertype_name" = parameter_type,
    "ts_name" = ts_name,
    "station_no" = station_number
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
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
  params <- list(
    "request" = "getTimeseriesValues",
    "ts_id" = ts_id,
    "from" = start_date,
    "to" = end_date,
    "returnfields" = paste(return_fields, collapse = ",")
  )

  get_bom_request <- make_bom_request(params)

  return(get_bom_request)
}
