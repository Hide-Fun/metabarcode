#' make unique number.
#'
#' @param .interval interval time.
#' @param .pattern reglex.
#' @export
make_unique_number = function(.interval, .pattern = "T+F|T+|.") {
  # integrate IT2 as a strings.
  strng <- paste0(.interval, collapse = "")
  # extract "TTT...F" and "F" and as_vector.
  df_det <- stringr::str_extract_all(strng, .pattern) %>%
    as_vector()
  # set uniqe number to each extract values.
  result <- vector("character", length(df_det))
  for (i in seq_along(df_det)){
    seq <- seq(1, length(df_det), by = 1)
    result[[i]] = stringr::str_c(rep(seq[[i]], str_length(df_det[[i]])), collapse = ".")
  }
  # covert strings to splite values and delimit by ".".
  unique_ID <- stringr::str_c(result, collapse = ".") %>%
    stringr::str_extract_all("\\d{1,3}") %>%
    purrr::as_vector()
  return(unique_ID)
}
