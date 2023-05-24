convert_types <- function(df) {
  cols <- names(df) != "station_no"
  df[cols] <- type.convert(df[cols], as.is = TRUE)
  df
}

collapse_params <- function(...) {
  params <- list(...)
  lapply(params, \(x) paste(x, collapse = ","))
}
