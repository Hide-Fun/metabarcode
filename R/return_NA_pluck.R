#' return_NA_pluck
#'
#' return_NA_pluck
#' @param .x list
#' @param ... element of list
#' @param .default return NA
#' @export
return_NA_pluck = function(.x, ..., .default = NA) {
  rlt <- pluck(.x = .x, ..., .default = .default)
  rlt <- unlist(rlt)
  return(rlt)
}
