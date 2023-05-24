#' @title Retrieve water observation stations
#' @md
#' @description
#' `get_station_list` queries Water Data Online and returns station details.
#' Queries can be input with the desired `parameter_type` to find all the
#' stations on record. If you already have a vector of station numbers, you can
#' pass the vector to `station_number` and return the details of those
#' stations.
#' `return_fields` can be customised to return various data about the stations.
#'
#' @param parameter_type The parameter for the station request (e.g. Water
#' Course Discharge, Storage Level)
#' @param station_number Optional: a single or multiple vector of AWRC station
#' numbers.
#' @param bbox Optional: a bounding box to get stations in a region of interest.
#' Takes a vector ordered xmin, ymin, xmax, ymax.
#' @param return_fields  Station details to be returned. By default the columns
#' returned are station name, number, ID, latitude and longitude. Can be
#' customised with a vector of parameters.
#' @return
#' With the default return fields, a tibble with columns station_name,
#' station_no, station_id, station_latitude, station_longitude.
#'
#' @details
#' Possible return fields for `get_station_list()` are:
#'
#' * station_name
#' * station_longname
#' * station_no
#' * station_id
#' * station_latitude
#' * station_longitude
#' * station_carteasting
#' * station_cartnorthing
#' * stationparameter_name
#' * station_georefsystem
#' * catchment_no
#' * catchment_id
#' * catchment_name
#' * site_no
#' * site_id
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * object_type
#' * custom_attributes
#'
#' @examples
#' # Get all Water Course Discharge Stations
#' \dontrun{
#' get_station_list()
#' }
#' # Just the details for Cotter River at Gingera
#' \dontrun{
#' get_station_list(station_number = "410730")
#' }
#' # Rainfall stations
#' \dontrun{
#' get_station_list(parameter_type = "Rainfall")
#' }
#' # Vector of return_fields
#' return_fields <- c(
#'   "station_name",
#'   "station_longname",
#'   "station_no",
#'   "station_id",
#'   "station_latitude",
#'   "station_longitude",
#'   "station_carteasting",
#'   "station_cartnorthing",
#'   "stationparameter_name",
#'   "station_georefsystem",
#'   "catchment_no",
#'   "catchment_id",
#'   "catchment_name",
#'   "site_no",
#'   "site_id",
#'   "site_name",
#'   "parametertype_id",
#'   "parametertype_name",
#'   "object_type",
#'   "custom_attributes"
#' )
#' # Get all attributes for one station
#' \dontrun{
#' get_station_list("Water Course Discharge", "410730", return_fields)
#' }
#' @export
get_station_list <- function(parameter_type = "Water Course Discharge",
                             station_number = NULL,
                             bbox = NULL,
                             return_fields = c("station_no",
                                               "station_name",
                                               "station_latitude",
                                               "station_longitude")
) {
  params <- list("request" = "getStationList")
  params[["parameterType_name"]] <- parameter_type
  if (!is.null(station_number)) {
    params[["station_no"]] <- paste(station_number, collapse = ",")
  }
  if (!is.null(bbox)) {
    bbox <- paste(bbox, collapse = ",")
    params[['bbox']] = bbox
  }
  params[["returnfields"]] <- paste(return_fields, collapse = ",")
  resp <- make_bom_request(params)
  convert_types(resp)
}

#' @title Retrieve available parameters for stations
#' @md
#' @description
#' `get_parameter_list` returns the parameters that can be retrieved at one or
#' more stations.
#' @param station_number A single or multiple vector of AWRC station
#' numbers.
#' @param return_fields (Optional) Station parameter details to be returned.
#' By default the return fields are: station_no, station_id, station_name,
#' parametertype_id, parametertype_name, parametertype_unitname
#' parametertype_shortunitname.
#' @details
#' The default return fields have been selected to generally return the most
#' useful fields while reducing duplication of metadata.
#' The full list of return fields:
#' * station_no',
#' * station_id
#' * station_name
#' * stationparameter_id
#' * stationparameter_no
#' * stationparameter_name
#' * stationparameter_longname
#' * site_id
#' * site_no
#' * site_name
#' * parametertype_id
#' * parametertype_name
#' * parametertype_longname
#' * parametertype_unitname
#' * parametertype_shortunitname
#' @return
#' A tibble with columns for each of the return fields.
#' @examples
#' # Return parameters for a single station
#' \dontrun{
#' get_parameter_list(station_number = "410730")
#' }
#' # Return available parameters for multiple stations
#' \dontrun{
#' get_parameter_list(station_number = c("410730", "570946"))
#' }
#' @export
get_parameter_list <- function(station_number,
                               return_fields = c(
                                 "station_no",
                                 "station_name",
                                 "parametertype_name",
                                 "parametertype_unitname"
                               )) {
  params <- list("request" = "getParameterList")
  params[["station_no"]] <- paste(station_number, collapse = ",")
  params[["returnfields"]] <- paste(return_fields, collapse = ",")
  resp <- make_bom_request(params)
  convert_types(resp)
}

convert_types <- function(df) {
  dplyr::mutate(
    df,
    dplyr::across(
      !dplyr::any_of("station_no"),
      \(x) utils::type.convert(x, as.is = TRUE)
  ))
}
