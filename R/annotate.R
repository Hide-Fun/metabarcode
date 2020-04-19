#' plot subplot
#'
#' make subplot
#' @param .sub_plot plot
#' @param .x place
#' @param .y place
#' @param .size size
#' @export
annotate_plot <- function(.sub_plot, .x, .y, .size) {
  grobs <- annotation_custom(ggplotGrob(.sub_plot),
                             xmin = .x - .size, xmax = .x + .size,
                             ymin = .y - .size, ymax = .y + .size)
}
