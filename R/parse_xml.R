#' Prepare xml
#'
#' @param .xml_list xml as_list()
#' @param .manual logical
#' @export
#' @examples
#' \dontrun{
#' # load file.
#' xml <- (read_xml("test.xml")
#'
#' # prepare
#' by_sample <- prepare(.xml = xml)
#'
#' # make table.
#' result <- map_dfr(.x = by_sample, .f = parse_xml, .id = "id") %>%
#'   pivot_wider(names_from = "name", values_from = "value")
#' }
parse_xml = function(.xml_list) {
  # extract base information.
  spec1 <- list(
    "GBSeq_definition", "GBSeq_length", "GBSeq_moltype",
    "GBSeq_primary-accession", "GBSeq_taxonomy", "GBSeq_organism", "GBSeq_sequence"
  )
  # make vector.
  value <- vector(mode = "character", length = length(spec1))
  # make tibble.
  base_info <- tibble::tibble(
    name = rep(NA_character_, length(spec1)),
    value = rep(NA_character_, length(spec1))
  )
  for(i in 1:length(spec1)) {
    value[i] <- return_NA_pluck(
      .x = .xml_list,
      "GBSeq", spec1[[i]]
    )
    base_info[i, "name"] <- spec1[[i]]
    base_info[i, "value"] <- value[i]
  }
  # extract title.
  spec2 <- list(
    "GBSeq",
    "GBSeq_references",
    "GBReference",
    "GBReference_title"
  )
  title_df <- tibble::tibble(
    name = "title",
    value = NA_character_
  )
  title <- return_NA_pluck(
    .x = .xml_list,
    spec2[[1]], spec2[[2]], spec2[[3]], spec2[[4]]
  )
  title_df[1, 2] <- title
  # extratct GBFeature.
  spec3 <- list(
    "GBSeq_feature-table", "GBFeature", "GBFeature_quals"
  )
  qual <- return_NA_pluck(
    .x = .xml_list,
    "GBSeq", spec3[[1]], spec3[[2]], spec3[[3]]
  )
  feature <- purrr::map(qual, posiible_parse_GBQualifier) %>%
    dplyr::bind_rows()
  rlt <- dplyr::bind_rows(
    base_info, title_df, feature)
  return(rlt)
}

