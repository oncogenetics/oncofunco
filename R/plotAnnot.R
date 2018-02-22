#' LocusExplorer - Annotation plot, collapsed
#'
#' Annotation plot for LocusExplorer.
#' @param data dataframe with c(CHR, BP, TYPE1, TYPE2, COLOUR_HEX, TYPE2N) columns.
#' @param chrom chromosome name, interger 1:23
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param vline Mark hits on genes
#' @param collapse Collapse TYPE2 group into TYPE1 group
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @export plotAnnot
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords locusexplorer annotation plot

plotAnnot <- function(data,
                      chrom = NULL,
                      xStart = NULL,
                      xEnd = NULL,
                      vline = NULL,
                      collapse = FALSE,
                      pad = TRUE){
  #subset data for zoomed region
  data <- as.data.frame(data)
  data <- data[ data$CHR == chrom &
                  data$BP >= xStart &
                  data$BP <= xEnd, ]
  
  if(collapse){ data$TYPE2N <- ifelse(data$TYPE1 == "ChromHMM", 3, data$TYPE2N) } 
  
  #plot
  gg_out <- 
    ggplot(data,
           aes(x = BP, xend = BP,
               y = TYPE2N - 1, yend = TYPE2N,
               col = COLOUR_HEX, fill = COLOUR_HEX )) +
    geom_segment(alpha = ifelse(collapse, 0.5, 1)) 
  
  if(collapse){
    gg_out <- gg_out +
      scale_y_continuous(
        limits = c(-1, 4),
        breaks = c(0:3) + 0.5, 
        labels = if(pad){
          strPadLeft(c("DNaseI", "Conserved", "ChromHMM", "eQTL")) } else {
            c("DNaseI", "Conserved", "ChromHMM", "eQTL")},
        name = expression(Annotation[]))
  } else {
    gg_out <- gg_out +
      scale_y_continuous(
        limits = c(-1, 11),
        breaks = c(0:10) + 0.5, 
        labels = if(pad){
          strPadLeft( c("DNaseI","Conserved",
                        #ChromHMM
                        "Heterochromatin","CTCF","CTCF+Enhancer","Promoter","Enhancer",
                        "Poised_Promoter","Transcribed","Repressed","CTCF+Promoter")) } else {
                          c("DNaseI","Conserved",
                            #ChromHMM
                            "Heterochromatin","CTCF","CTCF+Enhancer","Promoter","Enhancer",
                            "Poised_Promoter","Transcribed","Repressed","CTCF+Promoter") } ,
        name = expression(Annotation[]))
  }
  
  #prettify
  gg_out <- gg_out +
    scale_color_identity() +
    # general options
    coord_cartesian(xlim = c(xStart, xEnd))
  
  # mark hit SNPs - vline
  if(!is.null(vline)){
    gg_out <- gg_out +
      geom_vline(xintercept = vline,
                 col = "black", #col = "#4daf4a",
                 linetype = "dashed")
  }
  
  #return output ggplot
  return(gg_out)
} # END plotAnnot
