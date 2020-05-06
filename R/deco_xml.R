#' Extarct data from xml file
#'
#'
#' @param .GBFeature_quals GBFeature_quals
deco_xml = function(.GBFeature_quals) {
  GBqualifier <- list()
  for(i in 1:length(.GBFeature_quals)) {
    GBqualifier[[i]] <- .GBFeature_quals[[i]] %>% # individual
      xml2::xml_children()
  }
  return(GBqualifier)
}
