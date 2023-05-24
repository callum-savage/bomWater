#' Query the BOM WISKI API
#'
#' This function queries the Bureau of Meteorology (BOM) Water Data KISTERS API.
#' A parameter list is passed to make a the request and the JSON return is
#' parsed depending on what is requested. This function can be used if you want
#' to build your own JSON queries.
#'
#' @param params A named list of parameters.
#' @param max_tries The maximum number of times to retry the request, silently
#'   capped at 5 tries.
#'
#' @returns A tibble with columns depending on the request. For
#'   \code{\link{get_timeseries}} requests, a tibble with zero rows is returned
#'   if the query has no result.
#'
#' @export
make_bom_request <- function(params, max_tries = 1) {
  req <- construct_bom_req(params, max_tries)
  resp <- httr2::req_perform(req)
  tidy_bom_resp(resp, params$request)
}

construct_bom_req <- function(params, max_tries) {
  base_params <- list(
    "service" = "kisters",
    "type" = "QueryServices",
    "format" = "json"
  )
  req <- httr2::request("http://www.bom.gov.au/waterdata/services")
  req <- httr2::req_url_query(req, !!!c(base_params, params))
  req <- httr2::req_error(req, body = body_error)
  req <- httr2::req_retry(req, max_tries = min(max_tries, 5))
  req
}

tidy_bom_resp <- function(resp, request) {
  json <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (request == "getTimeseriesValues") {
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
