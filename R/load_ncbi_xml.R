#' laod fungal sequence and sample data from NCBI using accession ID
#'
#' @param .id character, Genbank accession ID.
#' @param .filename saved xml file name.
#' @export
load_ncbi_xml = function(.id, .filename) {
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
  utils::download.file(use_url,
                destfile = glue::glue(here::here("{.filename}")))
}
