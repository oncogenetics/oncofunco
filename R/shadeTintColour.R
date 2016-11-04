#' Shade and tints for input colour
#'
#' Shade and tints for input colour
#' @param col a character string with a valid colour name, see ?col2rgb for valid colour names. Defauls to "orange".
#' @param change numeric vector, negative values for tints, positive values for shades.
#' @return character vector of colour names.
#' @export shadeTintColour
#' @author Tokhir Dadaev
#' @seealso \url{http://colorbrewer2.org/}

shadeTintColour <- function(col = "orange", change = 25) {  
  # Example: shadeColor("#5CFF5C", 25)
  # positive change input = shade
  # negative change input = tints
  
  if(length(change) == 1) {
    RGB <- col2rgb(col)
    RGB <- RGB - RGB/100 * change
    RGB <- ifelse(RGB < 0, 0, RGB)
    RGB <- ifelse(RGB > 255, 255, RGB)
    res <- rgb(RGB[1], RGB[2], RGB[3], maxColorValue = 255)
  } else{
    res <- sapply(change, function(i){
      RGB <- col2rgb(col)
      RGB <- RGB - RGB/100 * i
      RGB <- ifelse(RGB < 0, 0, RGB)
      RGB <- ifelse(RGB > 255, 255, RGB)
      rgb(RGB[1], RGB[2], RGB[3], maxColorValue = 255)
      
    })
  } # END else
  
  return(res)
}