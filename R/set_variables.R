#' Creating formula.
#'
#' Creating formula for fitting multiple models.
#' @param .first_expl_var first explanatry variable
#' @param .other_expl_var explanatry variables
#' @param .resp_var responese variable
#' @param .type model type
#' @param .interaction interaction
#' @export
set_formulas = function(.first_expl_var, .other_expl_var, .resp_var, .type = "lm",
                        .interaction) {
  if(.interaction == T) {
    rlt <- .other_expl_var %>%
      purrr::accumulate(function(x, y) {glue::glue({x}, {y}, .sep = ' : ')},
                 .init = glue::glue({.resp_var}, " ", "~", " ",{.first_expl_var})) %>%
      purrr::set_names(1:length(.))
  } else{
   if(.type == "lm") {
     rlt <- .other_expl_var %>%
       purrr::accumulate(function(x, y) {glue::glue({x}, {y}, .sep = ' + ')},
                  .init = glue::glue({.resp_var}, " ", "~", " ",{.first_expl_var})) %>%
       purrr::set_names(1:length(.))
  }
  }
  rlt %>% tibble::enframe()
}


