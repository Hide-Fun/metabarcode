#' Extract data from NCBI xml file.
#'
#' This function make index_1.fasta and index_2.fasta from SampleSheet.csv.
#' @param .xml xml file
#' @param .id id file
#' @export
extract_ncbi = function(.xml, .id) {
  # extract sample information.
  GBFeature_quals <- xml2::xml_find_all(.xml, "//GBFeature_quals") # 106

  qualifier_bundle <- deco_xml(GBFeature_quals) # 106 contents

  info_nest_table <- purrr::map(.x = qualifier_bundle, ~deco_xml_tbl(.qualifier = .x))

  info_table <- info_nest_table %>%
    tibble::enframe() %>%
    dplyr::mutate(vec = purrr::map(.$value, ~pull(.data = .x, var = 1L)),
           is_organism = purrr::ap_int(vec, ~match(x = "organism", table = .x))) %>%
    dplyr::filter(!is.na(is_organism)) %>%
    tibble::rownames_to_column("num_b") %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider(names_from = item,
                values_from = value,
                values_fn = list(value = ~stringr::str_c(.,
                                                collapse = "_and_"))) %>%
    dplyr::select(-vec)

  seq <- xml2::xml_find_all(.xml, "//GBSeq_sequence") %>%
    xml_to_tibble()

  def <- xml2::xml_find_all(.xml, "//GBSeq_definition") %>%
    xml_to_tibble()

  accession_id <- xml2::xml_find_all(.xml, "//GBSeq_primary-accession") %>%
    xml_to_tibble()

  sample_feature <-
    dplyr::bind_rows(def, accession_id, seq) %>%
    tidyr::pivot_longer(-.item, names_to = "num_f", values_to = "value") %>%
    tidyr::pivot_wider(names_from = ".item", values_from = "value")

  sample_info <- dplyr::bind_cols(
    sample_feature, info_table
  ) %>%
    dplyr::right_join(.id, by = c("GBSeq_primary-accession" = "accession_id")) %>%
    dplyr::arrange(num_f)

  return(sample_info)
}
