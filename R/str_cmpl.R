#' Complement character.
#'
#' Complement character.
#' @param .id character
#' @param .gather max num
#' @param .fill filling character.
#' @param .prefix prefix
#' @param .suffix suffix
#' @param .sep separator.
#' @export
#' @examples
#' str_cmpl(.id = 2)
#' str_cmpl(.id = "Apple", .gather = 10, .fill = "A")
#' str_cmpl(.id = 34, .gather = 4, .fill = "9")
#' str_cmpl(.id = 12, .gather = 4, .fill = "0",
#'          .prefix = "suetsugulab", .sep = "-")
#' str_cmpl(.id = 12, .gather = 4, .fill = "0",
#'          .prefix = "suetsugulab-", .suffix = "A")
str_cmpl = function(.id, .gather = 4, .fill = "0",
                    .prefix = NULL, .suffix = NULL, .sep = "") {
  .id <- as.character(.id)
  # calculate length.
  len <- stringr::str_length(.id)
  compl <- .gather - len
  if(compl < 0) {
    stop(".gather must less than length of .id")
  }
  if(is.null(.prefix)&is.null(.suffix)) {
    rlt <- stringr::str_c(stringr::str_dup(.fill, compl), .id)
  } else if(is.null(.prefix)) {
    cmpl <- stringr::str_c(stringr::str_dup(.fill, compl), .id)
    rlt <- glue::glue(cmpl, "{.suffix}", .sep = .sep)
  } else if(is.null(.suffix)) {
    cmpl <- stringr::str_c(stringr::str_dup(.fill, compl), .id)
    rlt <- glue::glue("{.prefix}", cmpl, .sep = .sep)
  } else {
    cmpl <- stringr::str_c(stringr::str_dup(.fill, compl), .id)
    rlt <- glue::glue("{.prefix}", cmpl, "{.suffix}", .sep = .sep)
  }
  return(rlt)
}
