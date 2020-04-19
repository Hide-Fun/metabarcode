#' Make table for PCR 96 plate.
#'
#' @param .data input data.
#' @param .col_label column names
#' @param .plate_num 96 plate number
#' @export
PCR_table_96 = function(.data, .col_label, .plate_num) {
  .col_label <- enquo(.col_label)
  tidy <- data %>%
    dplyr::filter(Plate == .plate_num) %>%
    mutate(value = str_c("'", !!.col_label, "'")) %>%
    dplyr::select(value)

  if(nrow(tidy) != 96) {
    dummy <- tibble(!!.col_label := rep(NA, (96 - nrow(tidy))))
    tidy <- tidy %>%
      dplyr::select(value) %>%
      add_row(value = as_vector(dummy))
  }
  reshape <- tidy %>%
    mutate(row = rep(LETTERS[1:8], 12)) %>%
    group_by(row) %>%
    mutate(col = row_number())

  pcr_table <- reshape %>%
    pivot_wider(names_from = "col", values_from = "value")
  return(pcr_table)
}
