#' alculate time lag
#'
#' @param .time time
#' @param .col col
#' @param .fill logical
#' @param .order character, "top" or "end".
#' @export
time_lag = function(.time, .col, .fill, .order = "top") {
  # enquot
  time <- rlang::enquo(.time)
  col <- rlang::enquo(.col)
  # stop
  stopifnot(.order %in% c("top", "end"))
  # top or end.
  if(.order == "top") {
    dplyr::mutate(lag = dplyr::lag(!!time),
                  interval = abs(lag - time))
  } else {
    dplyr::mutate(lead = dplyr::lead(!!time),
                  interval = abs(lead - time))
  }
}
