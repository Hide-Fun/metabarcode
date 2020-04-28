#' Convert xml to tibble.
#'
#' @param .xml xml file
#' @export
xml_to_tibble = function(.xml) {
  name <- .xml %>%
    xml2::xml_name() %>%
    enframe() %>%
    rename(.item = value)
  value <- .xml %>%
    xml2::xml_text() %>%
    tibble::enframe() %>%
    dplyr::rename(.value = value)
  rlt <- dplyr::bind_cols(name, value) %>%
    dplyr::select(.item, .value) %>%
    tibble::rownames_to_column(".num")
  return(rlt %>%
           tidyr::pivot_wider(dplyr::everything(),
                       names_from = ".num",
                       values_from = ".value"))
}
