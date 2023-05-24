#' @title Query the BoM WISKI API
#' @description
#' This function queries the Bureau of Meteorology Water Data KISTERS API.
#' A parameter list is passed to make request and the JSON return is parsed
#' depending on what is requested. This function can be used if you want to
#' build your own JSON queries.
#' @param params A named list of parameters.
#' @param tries Number of times to query the API. Only a single call is made by
#' default, and the maximum number of tries is 5.
#' @return
#' A tibble is returned with the columns depending on the request. For
#' \code{get_timeseries} requests, a tibble with zero rows is returned
#' if there is no data available for that query.
make_bom_request <- function(params, tries = 1) {
  base_params <- list("service" = "kisters",
                      "type" = "QueryServices",
                      "format" = "json")
  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  req <- httr2::req_url_query(req, !!!c(base_params, params))
  req <- httr2::req_error(req, body = get_body_error)
  if (tries > 5) {
    warning("Warning: too many tries requested. Defaulting to 5 tries.")
    tries <- 5
  }
  req <- httr2::req_retry(req, max_tries = tries)

  resp <- httr2::req_perform(req)
  json <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # Convert response into a tidy tibble
  # TODO check for other request types (including 'unknown')
  if (params$request == "getTimeseriesValues") {
    resp_cols <- unlist(stringr::str_split(json$columns, ","))
    resp_data <- json$data[[1]]
  } else {
    resp_cols <- json[1,]
    resp_data <- json[-1, , drop = FALSE]
  }
  colnames(resp_data) <- resp_cols
  tbl <- tibble::as_tibble(resp_data)
  tbl
}



get_body_error <- function(resp) {
  httr2::resp_body_json(resp, simplifyVector = TRUE)$message
}
