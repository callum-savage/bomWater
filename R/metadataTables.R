#' Access the quality code conversion table
#'
#' `quality_codes()` returns a table which can be used to convert the numeric
#' data quality codes returned by the WISKI API into the human readable codes
#' used on Water Data Online.
#'
#' @returns
#' a tibble with three columns: Quality Code, BOM Quality Code, and Description
#' @export
#'
#' @examples
#' quality_codes()
quality_codes <- function() {
  tibble::tibble(
    `Quality Code` = as.integer(c(10, 90, 110, 140, 210)),
    `BOM Quality Code` = c("A", "B", "C", "E", "F"),
    `Description` = c(
      "The record set is the best available given the technology, techniques and monitoring objectives at the time of classification.",
      "The record set is compromised in its ability to represent the parameter accurately.",
      "The record set is an estimate.",
      "The ability of the record set to represent the monitored parameter accurately is not known.",
      "The record set is not of release quality or contains missing data."
    )
  )
}

#' Access the interplation types conversion table
#'
#' `interpolation_types()` returns a table which can be used to interpret the
#' interpolation types returned by the WISKI API.
#'
#' @return
#' a tibble with three columns: Interpolation Type, Name, and
#' Description
#' @export
#'
#' @examples
#' interpolation_types()
interpolation_types <- function() {
  tibble::tibble(
    `Interpolation Type` = as.integer(c(101, 102, 103, 104, 201, 202, 205, 206, 301, 302, 303, 304, 403, 404, 503, 504, 603, 604, 703, 704)),
    Name = c(
      "Continuous data points - No interpolation",
      "Continuous data points - Linear interpolation",
      "Continuous data points - Constant until next time stamp",
      "Continuous data points - Constant since previous time stamp",
      "Continuous totals - No interpolation",
      "Continuous totals - Linear interpolation",
      "Continuous totals - Linear until next time stamp",
      "Continuous totals - Linear since previous time stamp",
      "Continuous Directional Values - No interpolation",
      "Continuous Directional Values - Linear interpolation",
      "Continuous Directional Values - Constant until next time stamp",
      "Continuous Directional Values - Constant since previous time stamp",
      "Aggregated Minimum - Constant until next time stamp",
      "Aggregated Minimum - Constant since previous time stamp",
      "Aggregated Maximum - Constant until next time stamp",
      "Aggregated Maximum - Constant since previous time stamp",
      "Aggregated Means - Constant until next time stamp",
      "Aggregated Means - Constant since previous time stamp",
      "Aggregated Totals - Constant until next time stamp",
      "Aggregated Totals - Constant since previous time stamp"
    ),
    Description = c(
      "Spot samples, irregular sparse readings (such as groundwater readings or staff gauge readings). There is no expression possible between 2 values.",
      "Instantaneously recorded, continuous time series data.",
      "Mean value or constant instantaneous value. Examples for means are water level, flow recorded as mean values and examples for constant instantaneous values are pump rates and gate settings.",
      "Mean value related to the end of interval.",
      "Tipping buckets, between two tips a zero-total is returned.",
      "Instantaneously recorded continuous time series data.",
      "A total value that is stored at the beginning of the totalling interval. The total itself is assumed to grow linear until the next value.",
      "A total value that is stored at the end of the totalling interval. The total itself is assumed to grow linear since the previous value.",
      "Spot samples of directions (such as wind direction).",
      "Direction measurements with linear interpolation. The value type 'directional' expresses the wrap at 360 degrees.",
      "Constant direction until next time stamp of direction measurement (such as gate setting).",
      "Constant direction since previous time stamp of direction measurement (such as gate setting).",
      "The minimum is representative for the observation interval. The interval main time stamp is equidistant and is stored at the beginning of the interval.",
      "The minimum is representative for the observation interval. The interval main time stamp is equidistant and is stored at the end of the interval.",
      "The maximum is representative for the observation interval. The interval main time stamp is equidistant and is stored at the beginning of the interval.",
      "The maximum is representative for the observation interval. The interval main time stamp is equidistant and is stored at the end of the interval.",
      "The interval main time stamp is stored at the beginning of the interval. The mean is representative until the next time stamp.",
      "The interval main time stamp is stored at the end of the interval. The mean is representative since the previous time stamp.",
      "The interval main time stamp is stored at the beginning of the interval. The total is representative until the next time stamp.",
      "The interval main time stamp is stored at the end of the interval. The total is representative since the previous time stamp."
    )
  )
}
