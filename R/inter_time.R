#' Cariculate interval time
#'
#' Cariculate intercal time from camera survey.
#' @param .col time data (<dttm>, <date>)
#' @export
#' @examples
#' interval_time
interval_time = function(.col) {
  inter_time <- c()
  if (length(.col) == 1) {
    inter_time <- .col # if row number is 1, use time.
    print("Can't cariculate interval time, data have only one observation.")
  } else {
    for (i in 1:length(.col)){
      inter_time[i] <- as.numeric(.col[i] - .col[i + 1], units = "secs")
    }
    inter_time[length(.col)] <- as.numeric(.col[length(.col) - 1] - .col[length(.col)], units = "secs")
  }
  return(abs(inter_time))
}


