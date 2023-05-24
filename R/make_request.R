#' Query the BOM WISKI API
#'
#' This function queries the Bureau of Meteorology (BOM) Water Data KISTERS API.
#' A parameter list is passed to make a the request and the JSON return is parsed
#' depending on what is requested. This function can be used if you want to
#' build your own JSON queries.
#'
#' @param params A named list of parameters.
#' @param tries Number of times to query the API. Only a single call is made by
#'   default, and the maximum number of tries is 5.
#'
#' @returns A tibble with columns depending on the request. For
#'   \code{\link{get_timeseries}} requests, a tibble with zero rows is returned
#'   if the query has no result.
#'
#' @export
make_bom_request <- function(params, tries = 1) {
  if (!(tries %in% 1:5)) {
    warning("Invalid number of tries requested. Defaulting to 1.")
    tries <- 1
  }

  base_params <- list("service" = "kisters",
                      "type" = "QueryServices",
                      "format" = "json")

  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  req <- httr2::req_url_query(req, !!!c(base_params, params))
  req <- httr2::req_error(req, body = body_error)
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

  if (length(dim(resp_data)) == 2) {
    colnames(resp_data) <- resp_cols
    tbl <- tibble::as_tibble(resp_data)
  } else {
    tbl <- tibble::as_tibble(sapply(resp_cols, \(x) character()))
  }
  tbl
}

body_error <- function(resp) {
  httr2::resp_body_json(resp, simplifyVector = TRUE)$message
}
