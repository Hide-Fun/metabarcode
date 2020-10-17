#' parse_clustered_otu
#'
#' parse_clustered_otu
#' @param .clustered_otu clustered.otu.gz produced by Claident.
#' @param .col_names name of column.
#' @examples
#' clustered_otu <- data.frame(
#'    otu = c(">representative_otu1", "assigned_otu1", "assigned_otu2",
#'           ">representative_otu2", "assigned_otu1",
#'           ">representative_otu3")
#' )
#' parse_clustered_otu(.clustered_otu = clustered_otu,
#'                     .col_name = otu)
parse_clustered_otu = function(.clustered_otu, .col_name = X1) {
  .col_name <- rlang::enquo(.col_name)
  n <- .clustered_otu %>%
    dplyr::mutate(cont = stringr::str_detect(!!.col_name, ">")) %>%
    dplyr::filter(cont == FALSE) %>%
    nrow()
  rlt1 <- tibble::tibble(representative = rep(NA, nrow(.clustered_otu)),
                         assigned = rep(NA, nrow(.clustered_otu)))
  # assign each sequences.
  for(i in 1:nrow(.clustered_otu)) {
    val1 <- .clustered_otu[[1]][[i]]
    if(str_detect(val1, ">")) {
      rlt1[[1]][[i]] <- val1
    } else {
      rlt1[[2]][[i - 1]] <- val1
    }
  }
  # return(rlt1)
  # remove NA.
  rlt1_rmv <- rlt1 %>%
    dplyr::filter(!is.na(representative) | !is.na(assigned))
  # modify representative column.
  rlt2 <- rlt1_rmv
  for(i in 1:nrow(rlt2)) {
    val2 <- rlt2[[1]][[i]]
    if(is.na(val2)) {
      rlt2[[1]][[i]] <- rlt2[[1]][[i - 1]]
    }
  }
  # return(rlt2)
  # modify assigned column.
  rlt3 <- rlt2
  for( i in 1:nrow(rlt3)) {
    val3 <- rlt3[[2]][[i]]
    if(is.na(val3)) {
      rlt3[[2]][[i]] <- rlt3[[1]][[i]]
    }
  }
  rlt <- rlt3 %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~stringr::str_remove(., ">")))
  return(rlt)
}
