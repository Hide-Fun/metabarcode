#' Creating table for FUNGuild.
#'
#' Creating table for analysis of FUNGuild ( http://www.stbates.org/guilds/app.php ).
#' This function requir two tables, (1) summary_table of OTU/read_num, (2) identification of each OTU.
#' Names of output file is Funguild_DB.txt.
#' @param .otu_table sumary table.
#' @param .identify_list identifying result.
#' @examples
Funguild_DB = function(.otu_table, .identify_list) {
  table <- .otu_table
  group <-  .identify_list

  print(paste0("Row number of otu_table is", " ", nrow(table), "."))
  print(paste0("Row number of group is", " ", nrow(group), "."))

  if(any(colnames(group) %in% "query" == T)) {
    print("Error: identify_list has query column, you should rename query to OTU !!")
  } else{
    taxonomy <- group %>%
      dplyr::mutate_all(stringr::str_replace_na(.)) %>%
      tidyr::unite(col = "taxonomy", superkingdom:species, sep = ";", remove = T) %>%
      dplyr::mutate(taxonomy = stringr::str_replace_all(taxonomy, "NA", "unidentified"))
    Funguild_DB <- table %>%
      tidyr::pivot_longer(-samplename, names_to = "OTU", values_to = "read_num") %>%
      tidyr::pivot_wider(names_from = "samplename", values_from = "read_num") %>%
      dplyr::right_join(taxonomy, by = "OTU") %>%
      dplyr::select(-taxonomy, dplyr::everything())
    return(Funguild_DB)
  }
}
