#' Convert matrix to tibble.
#'
#' @param .mat matrix
#' @param .row logical, convert row to column.
#' @param .rowname character, rowname.
#' @export
matrix_to_tbl = function(.mat, .row, .rowname = NULL) {
  rlt <- .mat %>%
    as.data.frame()
  if(.row == T) {
    if(is.null(.rowname)) {
      stop("You must set .rownames")
    } else{
      rlt <- rlt %>%
        rownames_to_column(var = .rowname)
    }
  } else {
    rlt
  }
  rlt %>% as_tibble()
}

