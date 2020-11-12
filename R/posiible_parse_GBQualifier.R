#' Parse GBQualifier (possibly).
#'
#' This function use internally in parse_xml.
#' @param .GBQualifier GBQualifier
#' @export
#' @examples
#' GBFeature_quals <- list(GBQualifier = list(
#'                    "GBQualifier_name" = "organism",
#'                    "GBQualifier_value" = "Ceratobasidiaceae sp."))
#'
#' parse_GBQualifier(.GBQualifier = GBFeature_quals)
posiible_parse_GBQualifier <- purrr::possibly(
  parse_GBQualifier,
  otherwise = tibble::tibble(name = NA, value = NA)
)
