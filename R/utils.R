convert_types <- function(df, dont_convert = "station_no") {
  cols <- !(names(df) %in% dont_convert)
  df[cols] <- type.convert(df[cols], as.is = TRUE)
  df
}

collapse_params <- function(...) {
  params <- list(...)
  lapply(params, \(x) paste(x, collapse = ","))
}
