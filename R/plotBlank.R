#' Blank plot
#'
#' Blank plot for LocusExplorer, if data is missing blank output ggplot with text annotation.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param yLabel character string, label for Y axis, default "".
#' @param textAnnot character string for plot text annotation, default "No data"
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @export plotBlank
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object

plotBlank <- function(
  xStart = NULL,
  xEnd = NULL,
  yLabel = "",
  textAnnot = "No data",
  pad = TRUE){
  
  # Check input ---------------------------------------------------------------
  if(is.null(xStart) | is.null(xEnd))
    stop("xStart and/or xEnd missing, must be numeric class.")
  xRange <- c(xStart, xEnd)
  yRange <- c(0, 1)
  
  # plot blank
  ggplot() + 
    geom_blank() +
    annotate("text",
             x = xStart + (xEnd - xStart) / 2,
             y = 0.5,
             label = textAnnot) +
    coord_cartesian(xlim = xRange, ylim = yRange) +
    scale_y_continuous(breaks = c(0.5),
                       labels = if(pad) strPadLeft("") else "",
                       name = yLabel) +
    theme(axis.text.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank()) +
  scale_x_continuous(name = "")
  }