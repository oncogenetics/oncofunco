#' ICR colours
#'
#' Returns ICR brand colour Hex values
#' @param col numeric value to match rownumber, or Pallete name, or Colour name.
#' @param plot logical, defaults to FALSE
#' @return a \code{character vector} object with hex values
#' @keywords icr colour hex
#' @author Tokhir Dadaev
#' @export ICRcolours

ICRcolours <- function(col = "Primary", plot = FALSE){
  #ICR colours
  colDat <- read.table(text = "
                       Pallete	Colour	Hex	Pantone	CMYK	RGB
                       Primary	White	#FFFFFF	NA	NA	NA
                       Primary	Black	#000000	NA	NA	NA
                       Primary	Green	#C9DD03	NA	NA	NA
                       Primary	Yellow	#FFD602	NA	NA	NA
                       Primary	Orange	#F9A100	NA	NA	NA
                       Primary	Pink	#EE7EA6	NA	NA	NA
                       Primary	Red	#A71930	NA	NA	NA
                       Primary	Grey	#616365	NA	NA	NA
                       Secondary	Olive	#726E20	NA	NA	NA
                       Secondary	Damson	#6E273D	NA	NA	NA
                       Secondary	BrightRed	#F00034	NA	NA	NA
                       Secondary	LightGrey	#ADAFAF	NA	NA	NA
                       Secondary	Blue	#003D4C	NA	NA	NA
                       ", comment.char = "", header = TRUE, stringsAsFactors = FALSE)
  
  # Subset ------------------------------------------------------------------
  if(is.numeric(col) & col > 0 & col < nrow(colDat)){
    res <- colDat[ col, ]
  } else {
    res <- colDat[
      tolower(colDat$Pallete) %in% tolower(col) |
        tolower(colDat$Colour) %in% tolower(col), ]
    if(nrow(res) == 0) stop("Colour name didn't match")
  }
  
  # Plot colours ------------------------------------------------------------
  if(plot){
    #keep old par and set new par
    oldMar <- par()$mar
    oldmfrow <- par()$mfrow
    par(mar = c(2.1, 6, 4.1, 2.1),
        mfrow = c(2, 1))
    # Primary colours
    x <- colDat[ colDat$Pallete == "Primary", ]
    barplot(rep(1, nrow(x)),
            main = "Primary colours",
            horiz = TRUE,
            names.arg = x$Colour,
            las = 2,
            border = ifelse(x$Colour == "White",
                            "black",NA),
            col = x$Hex,
            axes = FALSE)
    # Secondary colours
    x <- colDat[ colDat$Pallete == "Secondary", ]
    barplot(rep(1, nrow(x)),
            main = "Secondary colours",
            horiz = TRUE,
            names.arg = x$Colour,
            las = 2,
            border = ifelse(x$Colour == "White",
                            "black",NA),
            col = x$Hex,
            axes = FALSE)
    
    #reassign old par
    par(mar = oldMar,
        mfrow = oldmfrow)
  }
  
  # Return Hex colour -------------------------------------------------------
  return(res$Hex)
}





