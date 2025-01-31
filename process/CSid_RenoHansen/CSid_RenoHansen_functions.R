


#' Moving window function
#'
#' @param i
#' @param nt_hw
#' @param tot_p
#'
#' @export
#'
walk <- function(i, nt_hw, tot_p) {
  if (i <= nt_hw) {
    w_sta <<- 1
    w_end <<- i + nt_hw
  } else if (i >= tot_p - nt_hw) {
    w_sta <<- i - nt_hw
    w_end <<- tot_p
  } else {
    w_sta <<- i - nt_hw
    w_end <<- i + nt_hw
  }
}
