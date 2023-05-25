# TODO document sources
#
#' @source
#' Interpolation types are detailed in the
#' BOM SOS2 manual](http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf)
#' (6.3.2, pg 40).
#'
#' @source
#' The conversion of SOS2 quality codes into BOM quality codes is from the
#' \href{http://www.bom.gov.au/waterdata/wiski-web-public/Guide\%20to\%20Sensor\%20Observation\%20Services\%20(SOS2)\%20for\%20Water\%20Data\%20\%20Online\%20v1.0.1.pdf}{BOM SOS2 manual}
#' (6.3.3, pg 41). Quality code descriptions are from
#' \href{http://www.bom.gov.au/water/hrs/qc_doc.shtml}{Streamflow data quality codes for Hydrologic Reference Stations},
#' which also contains additional information on how these codes were developed.
#'
# load


#' @title Available water parameters
#' @description
#' `bw_parameters` returns a vector of parameters that can be retrieved from
#' Water Data Online.
#' @param pars Optional: if empty all available parameters will be returned.
#' Alternatively, a vector of the continuous or discrete parameters can be
#' requested.
#' @return
#' A vector of parameters.
#' @details
#' The units of the parameters are as follows:
#'
#' * Water Course Discharge (m3/s)
#' * Water Course Level (m)
#' * Electrical conductivity at 25C (µS/cm)
#' * Turbidity (NTU)
#' * pH
#' * Water Temperature (ºC)
#' * Storage Volume (ML)
#' * Storage Level (m)
#' * Ground Water Level (m)
#' * Rainfall (mm)
#' * Evaporation (mm)
#' * Dry Air Temperature (ºC)
#' * Relative Humidity (%)
#' * Wind Speed (m/s)
#' @md
#' @seealso
#' * \url{http://www.bom.gov.au/waterdata/}
#' * \href{http://www.bom.gov.au/waterdata/wiski-web-public/Guide%20to%20Sensor%20Observation%20Services%20(SOS2)%20for%20Water%20Data%20%20Online%20v1.0.1.pdf}{BoM Guide to Sensor Observation Services (SOS2) for Water Data Online}
#' @export
#' @examples
#' bw_parameters()
#' bw_parameters("continuous")
#' bw_parameters("discrete")
bw_parameters <- function(continuity = NULL) {
  if (is.null(continuity)) {
    bw_parameters_data$parameter
  } else if (continuity %in% c("continuous", "discrete")) {
    dplyr::filter(bw_parameters_data, continuity == !!continuity)$parameter
  } else {
    stop("continuity must one of continutous, discrete, or NULL")
  }
}


#' Access the bomWater quality code conversion table
#'
#' `quality_codes()` returns a table which can be used to convert the numeric
#' data quality codes returned by the WISKI API into the human readable codes
#' used on Water Data Online.
#' @returns A tibble with three columns: Quality Code, BOM Quality Code, and
#'   Description.
#' @export
#' @examples
#' quality_codes()
bw_quality_codes <- function() {
  bw_quality_codes_data
}

#' Access the interplation types conversion table
#'
#' `interpolation_types()` returns a table which can be used to interpret the
#' interpolation types returned by the WISKI API.
#' @return A tibble with three columns: Interpolation Type, Name, and
#'   Description.
#' @examples
#' interpolation_types()
#' @export
bw_interpolation_types <- function() {
  bw_interpolation_types_data
}


bw_return_fields <- function(request) {
  # get station list
  # return_fields <- c(
  #   "station_name",
  #   "station_longname",
  #   "station_no",
  #   "station_id",
  #   "station_latitude",
  #   "station_longitude",
  #   "station_carteasting",
  #   "station_cartnorthing",
  #   "stationparameter_name",
  #   "station_georefsystem",
  #   "catchment_no",
  #   "catchment_id",
  #   "catchment_name",
  #   "site_no",
  #   "site_id",
  #   "site_name",
  #   "parametertype_id",
  #   "parametertype_name",
  #   "object_type",
  #   "custom_attributes"
  # )

  #get_parameter_list

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

}
