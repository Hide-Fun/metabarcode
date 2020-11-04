#' Prepare list for parese_xml()
#'
#'
#' @param .xml
#' @export
prepare = function(.xml) {
  # convert to list.
  .xml_list <- xml2::as_list(.xml)
  # split by sample.
  ## make vector.
  by_sample <- vector("list", length(.xml_list[["GBSet"]]))
  ## split.
  for(i in 1:length(.xml_list[["GBSet"]])) {
    by_sample[[i]] <- .xml_list[["GBSet"]][[i]]
  }
  return(by_sample)
}
