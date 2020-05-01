#' Extract data from xml file
#'
#' @param .qualifier .qualifier
#' @export
deco_xml_tbl = function(.qualifier) {
  table <- list()
  for(i in 1:length(.qualifier)) {
    table[[i]] <- .qualifier[[i]] %>%
      xml2::xml_contents() %>% # extract contents.
      xml2::as_list() %>%
      unlist() %>%
      tibble::enframe() %>%
      tidyr::pivot_wider(dplyr::everything(),
                  names_from = "name",
                  values_from = "value")
  }
  rlt <- dplyr::bind_rows(table) %>%
    dplyr::rename(item = `1`, value = `2`)
  return(rlt)
}
