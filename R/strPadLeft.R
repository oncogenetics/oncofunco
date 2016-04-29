#' Left pad strings.
#'
#' Left pad strings.
#' @export strPadLeft
#' @import stringr

strPadLeft <- function(labels, width = 15, pad = " "){
  # ref: Used to left pad strings for manhattan plot on Yaxis.
  stringr::str_pad(string = labels,
                   width = width,
                   side = "left",
                   pad = pad)}




