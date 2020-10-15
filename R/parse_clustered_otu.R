#' parse_clustered_otu
#'
#' parse_clustered_otu
#' @param .df data.frame, tibble
#' @export
parse_clustered_otu = function(.df) {
  n <- .df %>%
    mutate(cont = str_detect(X1, ">")) %>%
    filter(cont == FALSE) %>%
    nrow()
  rlt1 <- tibble(representative = rep(NA, nrow(.df)),
                 assigned = rep(NA, nrow(.df)))
  # assign each sequences.
  for(i in 1:nrow(.df)) {
    val1 <- .df[[1]][[i]]
    if(str_detect(val1, ">")) {
      rlt1[[1]][[i]] <- val1
    } else {
      rlt1[[2]][[i - 1]] <- val1
    }
  }
  # return(rlt1)
  # remove NA.
  rlt1_rmv <- rlt1 %>%
    filter(!is.na(representative) | !is.na(assigned))
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
    mutate(across(everything(), ~str_remove(., ">")))
  return(rlt)
}
