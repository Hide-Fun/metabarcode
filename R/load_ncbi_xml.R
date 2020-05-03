#' Load sequence and sample infomation from NCBI by using E-utilities.
#'
#' laod fungal sequence and sample data from NCBI using accession ID.
#' You should keep to the regulations of E-utilities.
#' @param .id character, Genbank accession ID.
#' @param .filename saved xml file name.
#' @param .term logical, you understand E-Utilities term ?
#' @examples
load_ncbi_xml = function(.id, .filename = "download.xml", .term = F) {
  if(.term == F) {
    stop()
  } else {
    id_list <- .id %>%
      dplyr::select(accession_id) %>%
      dplyr::pull()
    # accession id
    .ids <- stringr::str_c(id_list, collapse = ",")
    # url
    base_url <-
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id={.ids}&rettype=gb&retmode=xml"
    # add accession id.
    use_url <- glue::glue(base_url)
    # download.
    utils::download.file(use_url,
                         destfile = here::here(.filename))
    Sys.sleep(0.5)
  }
}
