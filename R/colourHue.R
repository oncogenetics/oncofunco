#' ggplot2 colours
#'
#' Generate colours matching ggplot
#' @export colourHue

colourHue <- function(n){
  # adapted from http://stackoverflow.com/questions/8197559
  # ref: SNP LD colours ggplot default colours
  
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]}
