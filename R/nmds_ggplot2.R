#' Parce output of metaMDS for ggplot2.
#'
#' @param .data data
#' @param .key rowname
#' @param .sample sample data.
#' @param .nest logical
#' @param .col_name column name to extract
#' @export
nmds_ggplot2 = function(.data, .key, .sample, .nest = F, .col_name= "mds") {
  if(.nest == T) {
    mds <- flatten(.data[[.col_name]])
    point <- mds$point %>%
      as.data.frame() %>%
      rownames_to_column(.key) %>%
      as_tibble()
    # merge with sample_info_merge.
    point_merge <- point %>%
      left_join(.sample, by = .key)
  } else {
    point <- mds$point %>%
      as.data.frame() %>%
      rownames_to_column(.key) %>%
      as_tibble()
    # merge with sample_info_merge.
    point_merge <- point %>%
      left_join(.sample, by = .key)
  }
  return(point_merge)
}
