#' ggplot2 colours
#'
#' Generate colours matching ggplot. Adapted from JohnColby's solution at StackOverflow.
#' @param n number of colours to output.
#' @return n number of colour names.
#' @export colourHue
#' @author Tokhir Dadaev
#' @seealso \url{https://stackoverflow.com/a/8197703/680068}

colourHue <- function(n){
  # adapted from https://stackoverflow.com/a/8197703/680068
  # ref: SNP LD colours ggplot default colours
  
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]}
