#' Creating text file for FUNGuild.
#'
#' If your identifying data (e.g, "combined.txt") dose not have col_names, this function automatically name col_names (taxonomic levels).
#' So, you should carefully check creating col_names is appropriate or not.
#' @param table sumary table like "summary_nonchimeras.txt"
#' @param combined identifying result like "combined.txt"
#' @param combined_col_names logical. Default is TRUE. This parameter determins ignoreing col_names of combined.txt or not.
#' @export
#' @examples
#' example
funguild_DB = function(table, combined, combined_col_names = T){
  table = readr::read_tsv(table)
  if (combined_col_names == T) {
    identify_list = readr::read_tsv(combined,
                                    col_names = T)
  } else {
    identify_list = readr::read_tsv(combined,
                                    col_names = F)
    identify_list = identify_list %>%
        dplyr::rename(OTU = X1, Domain = X2, Kingdom = X3, subkingdom = X4, Phylum = X5, Class = X6, subclass = X7,
                      Order = X8, Family = X10, subfamily = X11, Tribe = X12, Genus = X14, spceies = X15)
  }
  Fungal_OTU = identify_list$OTU
  readr::write_delim(identify_list %>% select(-OTU),
                     "identify_list_rmv.txt",
                     delim = ";")
  taxonomy = readr::read_tsv("identify_list.txt") %>%
    dplyr::rename(taxonomy = `Domain;Kingdom;subkingdom;Phylum;Class;subclass;Order;X9;Family;subfamily;Tribe;X13;Genus;spceies`) %>%
    dplyr::mutate(taxonomy = str_replace_all(taxonomy, "NA", "unidentified")) %>%
    tibble::add_column(OTU = Fungal_OTU)
  Funguild_DB = table %>%
    tidyr::pivot_longer(-samplename, names_to = "OTU", values_to = "read_num") %>%
    dplyr::right_join(taxonomy, by = "OTU") %>%
    tidyr::pivot_wider(names_from = "samplename", values_from = "read_num") %>%
    dplyr::select(-taxonomy, everything())
  readr::write_tsv(Funguild_DB, "Funguild_DB.txt")

}
