#' Parse GBQualifier
#'
#' This function use internally parse_xml.
#' @param .GBQualifier
#' @export
#' @examples
#' GBFeature_quals <- list(GBQualifier = list(
#'                    "GBQualifier_name" = "organism",
#'                    "GBQualifier_value" = "Ceratobasidiaceae sp."))
#'
#' parse_GBQualifier(.GBQualifier = GBFeature_quals)
parse_GBQualifier = function(.GBQualifier = list()) {
  .GBQualifier <- flatten(.GBQualifier)
  rlt <- tibble::tibble(
    name = .GBQualifier[[1]],
    value = .GBQualifier[[2]]
  )
  return(rlt)
}
