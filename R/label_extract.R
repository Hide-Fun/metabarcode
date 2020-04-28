#' Extracting label number.
#'
#' this function extraxt number from chracter.
#' accepting number type is "2-10", "1-9-3", "1-9-10", "3" ...etc.
#' @param string A character
#' @export
#' @examples
#' label_extract("ABC_2-1-10_DE")
label_extract = function(string) {
  stopifnot(length(string) == 1)
  if (!is.character(string)) {
    stop("string must be a character")
  }
  if (stringr::str_detect(string, "\\d+-\\d+-\\d+")) {
    stringr::str_extract(string, "\\d+-\\d+-\\d+")
  } else if (stringr::str_detect(string, "\\d+-\\d+")) {
    stringr::str_extract(string, "\\d+-\\d+")
  } else {
    stringr::str_extract(string, "\\d+")
  }
}
