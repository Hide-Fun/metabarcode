#' Convert table to fasta.
#'
#' @param .data table
#' @param .otu otu column name.
#' @param .seq sequence column name.
#' @export
table_to_fasta = function(.data, .otu, .seq) {
  otu <- enquo(.otu)
  seq <- enquo(.seq)
  # select
  n <- nrow(.data)
  otu_col <- .data %>%
    dplyr::select(!!otu) %>%
    rename(col = !!otu) %>%
    mutate(col = stringr::str_c(">", col)) %>%
    add_column(num = seq(1, 2 * n, by = 2))
  seq_col <- .data %>%
    dplyr::select(!!seq) %>%
    rename(col = !!seq) %>%
    add_column(num = seq(1, 2 * n, by = 2) + 1)
  # bind
  fasta <- bind_rows(otu_col, seq_col) %>%
    arrange(num) %>%
    dplyr::select(col)
  return(fasta)
}
