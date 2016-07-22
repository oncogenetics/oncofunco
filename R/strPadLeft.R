#' Left pad strings.
#'
#' Left pad strings.
#' @export strPadLeft
#' @author Tokhir Dadaev


strPadLeft <- function(labels, width = 15, pad = " "){
  # ref: Used to left pad strings for manhattan plot on Yaxis.
  
  #ensure 1 nchar
  pad <- substr(pad,1,1)
  
  # left pad with pad character
  x <- paste0(sapply(width - ifelse(nchar(labels) > width,
                                    width,
                                    nchar(labels)),
                     function(i) paste0(rep(pad,i), collapse = "")),
              labels)
  
  #cut longer than width vars
  return(substr(x, 1, width))
  
  # @import stringr
  # stringr::str_pad(string = labels,
  #                  width = width,
  #                  side = "left",
  #                  pad = pad)}
  # labels <- c("1", "12", "123", "1234", "12345")
}
