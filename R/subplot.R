#' Make sub plot
#'
#' @param .data data
#' @param .aes aes
#' @param .stat stat
#' @param .pie logical, make pie chart.
#' @export
my_sub_plot <- function(.data, .aes, .stat, .pie) {
  g <- ggplot(.data, .aes) +
    geom_bar(stat = .stat) +
    theme_void() +
    scale_fill_manual(values = .data$color, guide = F)
  if(.pie == T) {
    coord_polar("y")
  }
}
