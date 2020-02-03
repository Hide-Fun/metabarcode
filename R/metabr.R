#' Title of the simple function to extract_num.
#'
#' The second paragraph is recognized as the description.
#' It is not recommended to use @@title and @@description explicitly.
#' @param string A character
#' @export tidyverse
#' @examples
#' extract_num(ABD_2-1-10_GG)
extract_num = function(string) {
  if (str_detect(string, "\\d{1,2}-\\d{1,2}-\\d{1,2}")) {
    str_extract(string, "\\d{1,2}-\\d{1,2}-\\d{1,2}")
  } else if (str_detect(string, "\\d{1,2}-\\d{1,2}")) {
    str_extract(string, "\\d{1,2}-\\d{1,2}")
  } else {
    str_extract(string, "\\d{1,2}")
  }
}
