#' Title of the simple function to extract_num.
#'
#' The second paragraph is recognized as the description.
#' It is not recommended to use @@title and @@description explicitly.
#' @param string A character
#' @export
#' @examples
#' extract_num(ABD_2-1-10_GG)
label_extract = function(string) {
  stopifnot(length(string) == 1)
  if (!is.character(string)) {
    stop("string must be a character")
  }
  if (stringr::str_detect(string, "\\d{1,2}-\\d{1,2}-\\d{1,2}")) {
    stringr::str_extract(string, "\\d{1,2}-\\d{1,2}-\\d{1,2}")
  } else if (stringr::str_detect(string, "\\d{1,2}-\\d{1,2}")) {
    stringr::str_extract(string, "\\d{1,2}-\\d{1,2}")
  } else {
    stringr::str_extract(string, "\\d{1,2}")
  }
}
