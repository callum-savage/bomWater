#' List water observation stations
#'
#' `get_station_list` queries Water Data Online and returns station details.
#' Queries can be input with the desired `parameter_type` to find all the
#' stations on record. If you already have a vector of station numbers, you can
#' pass the vector to `station_number` and return the details of those stations.
#' `return_fields` can be customised to return various data about the stations.
#'
#' @param parameter_type The parameter for the station request. Defaults to
#'   "Water Course Level".
#' @param station_number Optional: a vector of AWRC station numbers.
#' @param bbox Optional: a bounding box to get stations in a region of interest.
#'   Takes a vector ordered xmin, ymin, xmax, ymax.
#' @param return_fields A character vector of station details to be returned.
#'   See [return_fields()] for a full list of available fields.
#' @return A tibble with columns gven by return_fields.
#'
#' @examples
#' # Get a list of all Water Course Discharge Stations
#' \dontrun{
#' get_station_list(parameter_type = "Water Course Discharge")
#' }
#' # Just get the details for Cotter River at Gingera
#' \dontrun{
#' get_station_list(station_number = "410730")
#' }
#' # Get all attributes for one station
#' \dontrun{
#' all_discharge_fields <- return_fields("Water Course Discharge")
#' get_station_list("Water Course Discharge", "410730", return_fields = all_discharge_fields)
#' }
#' @export
get_station_list <- function(parameter_type = "Water Course Level", # water_course_level,
                             station_number = NULL,
                             bbox = NULL,
                             return_fields = c("station_no",
                                               "station_name",
                                               "station_latitude",
                                               "station_longitude")
) {
  params <- collapse_params(
    request = "getStationList",
    parameterType_name = parameter_type,
    station_no = station_number,
    bbox = bbox,
    returnfields = return_fields
  )
  resp <- make_bom_request(params)
  convert_types(resp)
}

#' @title Retrieve available parameters for stations
#' @md
#' @description `get_parameter_list` returns the parameters that can be
#' retrieved at one or more stations.
#' @param station_number A single or multiple vector of AWRC station numbers.
#' @param return_fields (Optional) Station parameter details to be returned. By
#'   default the return fields are: station_no, station_id, station_name,
#'   parametertype_id, parametertype_name, parametertype_unitname
#'   parametertype_shortunitname.
#' @details The default return fields have been selected to generally return the
#' most useful fields while reducing duplication of metadata. The full list of
#' return fields:
#' @return A tibble with columns for each of the return fields.
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
  params <- collapse_params(
    request = "getParameterList",
    station_no = station_number,
    returnfields = return_fields
  )
  resp <- make_bom_request(params)
  convert_types(resp)
}

# TODO return custom attributes as a nested data frame?
get_data_owner <- function(parameter_type, station_number) {
  station_list <- get_station_list(
    parameter_type = parameter_type,
    station_number = station_number,
    return_fields = c("station_no", "custom_attributes")
  )
  if (nrow(station_list) == 0) {
    stop(paste("Station number", station_number, "is invalid"))
  } else if (!("DATA_OWNER" %in% names(station_list))) {
    warning(paste("Unable to find data owner for station ", station_number))
    return(character())
  }
  station_list$DATA_OWNER_NAME
}
