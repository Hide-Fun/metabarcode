#' Convert tibble to matrix.
#'
#' Convert tibble to matrix.
#' @param .tbl tibble
#' @param .row logical. Want rownames ?
#' @param .rowname character.
#' @export
tibble_to_matrix = function(.tbl, .row = F, .rowname = NULL) {
  if(.row == T) {
    if(is.null(.rowname)) {
      stop("You must set rowname.")
    } else {
      rlt <- .tbl %>%
        tibble::column_to_rownames(.rowname)
    }
  } else {
    rlt <- .tbl %>%
      as.data.frame()
  }
  rlt_2 <- rlt %>%
    as.matrix()
  return(rlt_2)
}
