#' Making Index fast file.
#'
#' The second paragraph is recognized as the description.
#' It is not recommended to use @@title and @@description explicitly.
#' @param filename "SampleSheet.csv"
#' @param skip number of skip row, default 18
#' @export
#' @examples
#' extract_num(ABD_2-1-10_GG)
index_make = function(filename, skip = 18) {
  samplesheet = readr::read_csv("filename", skip = skip)
  index = dplyr::tibble(ID = samplesheet$Sample_Name,
                        index_1 = samplesheet$index,
                        index_2 = samplesheet$index2) %>%
    dplyr::mutate(ID = str_c(">", ID)) %>%
    tibble::add_column(index_num = seq(length.out = nrow(samplesheet), by = 2) + 1,
                       ID_num = seq(length.out = nrow(samplesheet), by = 2))
  index_arrange = dplyr::tribble(
    ~num, ~index_1, ~index_2,
    index$index_num, index$index_1, index$index_2,
    index$ID_num, index$ID, index$ID) %>%
    tidyr::unnest(cols = c(num, index_1, index_2)) %>%
    dplyr::arrange(num)
  readr::write_tsv(index_arrange[,2], col_names = F, "index_1.fasta")
  readr::write_tsv(index_arrange[,3], col_names = F, "index_2.fasta")
}
