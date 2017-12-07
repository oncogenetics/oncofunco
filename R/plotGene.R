#' Plot genes using ggbio::geom_alignment
#'
#' This function returns GRanges object, used as input for plotting gene track
#' Warning: too much dependency to other packages, will need to rewrite/update the code in future far far away...
#' Warning: only works for LE at the moment with pre-loaded bio packages
#' @param chrom chromosome name, must be character class with length(chrom)==1, e.g.: chr1"
#' @param chromStart,chromEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param vline Mark hits on genes
#' @keywords gene symbol granges plot
#' @export plotGene
#' @author Tokhir Dadaev
#' @return a list with 2 objects: $genePlot \code{ggplot} object and $geneCount numeric

plotGene <- function(chrom = NULL,
                     chromStart = NULL,
                     chromEnd = NULL,
                     vline = NULL){
  plotDatGeneN <- 1

  #get granges collapsed genes for ggplot+ggbio
  plotDatGene <- geneSymbol(chrom, chromStart, chromEnd)
  if(is.null(plotDatGene)){
    gg_out <- ggplot() +
      geom_blank() +
      annotate("text",
               x = chromStart + (chromEnd - chromStart)/2,
               y = 0.35,
               label = "No gene") +
      #general options
      ylab("") +
      xlim(c(chromStart, chromEnd)) +
      theme(legend.position = "none",
            panel.background = element_rect(fill="white"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.border = element_blank(),
            #Y Axis font
            axis.text = element_blank())
  } else {

    #number of genes in zoomed region, if no genes then 1
    plotDatGeneN <- try({
      length(unique(plotDatGene@elementMetadata$gene_id))}, silent = TRUE)
    if(class(plotDatGeneN) == "try-error"){ plotDatGeneN <- 1 }


    # return ggbio:gene plot
    gg_out <- ggplot() +
      geom_hline(yintercept = c(1:plotDatGeneN),
                 col = "grey60", linetype = "dotted") +
      #ggbio plot genes
      geom_alignment(data = plotDatGene,aes(group = gene_id,
                                            fill = strand, col = strand)) +
    # mark hit SNPs - vline
    if(!is.null(vline)){
      gg_out <- gg_out +
        geom_vline(xintercept = vline,
                   col = "black", #col = "#4daf4a",
                   linetype = "dashed")
    }
  }
  #return output ggplot
  return(list(genePlot = gg_out,
              geneCnt = plotDatGeneN))


} # END plotGene
