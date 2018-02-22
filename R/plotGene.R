#' Plot genes using ggbio::geom_alignment
#'
#' This function returns GRanges object, used as input for plotting gene track
#' Warning: too much dependency to other packages, will need to rewrite/update the code in future far far away...
#' Warning: only works for LE at the moment with pre-loaded bio packages
#' @param chrom chromosome name, must be character class with length(chrom)==1, e.g.: chr1"
#' @param chromStart,chromEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param hits Default NULL. Color genes based on strand and hits.
#' @param vline Mark hits on genes
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @keywords gene symbol granges plot
#' @export plotGene
#' @author Tokhir Dadaev
#' @return a list with 2 objects: $genePlot \code{ggplot} object and $geneCount numeric


plotGene <- function(chrom = NULL,
                     chromStart = NULL,
                     chromEnd = NULL,
                     hits = NULL,
                     vline = NULL,
                     pad = TRUE){
  
  plotDatGeneN <- 1
  
  #get granges collapsed genes for ggplot+ggbio
  plotDatGene <- geneSymbol(chrom, chromStart, chromEnd)
  
  if(is.null(plotDatGene)){
    gg_out <- ggplot() +
      geom_blank() +
      annotate("text",
               x = chromStart + (chromEnd - chromStart)/2,
               y = 0.35,
               label = "No gene"
      )
  } else {
    
    #number of genes in zoomed region, if no genes then 1
    plotDatGeneN <- try({
      length(unique(plotDatGene@elementMetadata$gene_id))}, silent = TRUE)
    if(class(plotDatGeneN) == "try-error"){ plotDatGeneN <- 1 }
    
    # Color genes on hits or on strand: -, *, +
    # > ICRcolours("blue")
    # [1] "#003D4C"
    # > ICRcolours("Grey")
    # [1] "#616365"
    # > ICRcolours("Olive")
    # [1] "#726E20"
    # > ICRcolours("BrightRed")
    # [1] "#F00034"
    
    lookUpCol <- setNames(c("#003D4C", "#616365", "#726E20"), c("-", "*", "+"))
    plotDatGene@elementMetadata$Fill <-
      ifelse(plotDatGene@elementMetadata$gene_id %in% hits,
             "#F00034",
             lookUpCol[ as.character(plotDatGene@strand) ])
    
    
    # pad gene names to align
    if(pad) {
      plotDatGene@elementMetadata$gene_id <-
        oncofunco::strPadLeft(plotDatGene@elementMetadata$gene_id)}
    
    # return ggbio:gene plot
    gg_out <- ggplot() +
      geom_hline(yintercept = c(1:plotDatGeneN),
                 col = "grey60", linetype = "dotted") +
      #ggbio plot genes
      geom_alignment(data = plotDatGene, aes(group = gene_id,
                                             fill = Fill, col = Fill)) +
      scale_color_identity() +
      scale_fill_identity() +
      ylab(expression(Gene[]))
    # mark hit SNPs - vline
    if(!is.null(vline)){
      gg_out <- gg_out +
        geom_vline(xintercept = vline,
                   col = "black", #col = "#4daf4a",
                   linetype = "dashed")}
    
    
  }
  
  gg_out <- gg_out + coord_cartesian(xlim = c(chromStart, chromEnd))
  
  #return output ggplot
  return(list(genePlot = gg_out,
              geneCnt = plotDatGeneN))
  
  
} # END plotGene



#  Version without gene colours --------------------------------------------
# plotGene <- function(chrom = NULL,
#                      chromStart = NULL,
#                      chromEnd = NULL,
#                      vline = NULL,
#                      pad = TRUE){
#   plotDatGeneN <- 1
# 
#   #get granges collapsed genes for ggplot+ggbio
#   plotDatGene <- geneSymbol(chrom, chromStart, chromEnd)
#   
#   if(is.null(plotDatGene)){
#     gg_out <- ggplot() +
#       geom_blank() +
#       annotate("text",
#                x = chromStart + (chromEnd - chromStart)/2,
#                y = 0.35,
#                label = "No gene")
#   } else {
# 
#     #number of genes in zoomed region, if no genes then 1
#     plotDatGeneN <- try({
#       length(unique(plotDatGene@elementMetadata$gene_id))}, silent = TRUE)
#     if(class(plotDatGeneN) == "try-error"){ plotDatGeneN <- 1 }
# 
#     # pad gene names to align
#     if(pad) {
#       plotDatGene@elementMetadata$gene_id <-
#         oncofunco::strPadLeft(plotDatGene@elementMetadata$gene_id)}
#     
#     # return ggbio:gene plot
#     gg_out <- ggplot() +
#       geom_hline(yintercept = c(1:plotDatGeneN),
#                  col = "grey60", linetype = "dotted") +
#       #ggbio plot genes
#       geom_alignment(data = plotDatGene, aes(group = gene_id,
#                                              fill = strand, col = strand)) +
#       ylab(expression(Gene[]))
#     # mark hit SNPs - vline
#     if(!is.null(vline)){
#       gg_out <- gg_out +
#         geom_vline(xintercept = vline,
#                    col = "black", #col = "#4daf4a",
#                    linetype = "dashed")}
#     
#     
#   }
#   
#   gg_out <- gg_out + coord_cartesian(xlim = c(chromStart, chromEnd))
#     
#   #return output ggplot
#   return(list(genePlot = gg_out,
#               geneCnt = plotDatGeneN))
# 
# 
# } # END plotGene
