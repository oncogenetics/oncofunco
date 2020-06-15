#' LocusExplorer - TCGA eQTL plot
#'
#' TCGA eQTL plot for LocusExplorer.
#' @param data data.frame with columns: c("SNP", "gene", "xSNP", "xGene")
#' @param xStart,xEnd Region range, minimum and maximum position on x axis, advised to keep this less than 5Mb.
#' @param hits SNP names to label in the plot. Must be present in data data.frame.
#' @param hitsTags Tag SNP names to label in the plot. Must be present in data data.frame.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param title character string for plot title. Default is NULL, i.e.: no plot title. 
#' @export plotEQTL
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords eQTL TCGA plot SNP genetics

plotEQTL <- function(
  data = NULL,
  xStart = NULL,
  xEnd = NULL,
  hits = NULL,
  hitsTags = TRUE,
  label = "eQTL TCGA",
  pad = TRUE,
  title = NULL){
  
  # Check input data --------------------------------------------------------
  if(is.null(data)) stop("data is missing, must have columns: c('SNP', 'gene', 'xSNP', 'xGene')")
  if(!all(c(c("SNP", "gene", "xSNP", "xGene")) %in% colnames(data))){
    stop("data must have columns: c('SNP', 'gene', 'xSNP', 'xGene')") 
  } else data <- as.data.frame(data)
  
  #data = as.data.frame(datEQTL)
  data$type = factor(ifelse(data$SNP %in% hits, "hit",
                           ifelse(data$SNP %in% hitsTags, "hitTag", "other")),
                    levels = c("hit", "hitTag", "other"))
  data$linetype = ifelse(data$type %in% c1, "solid", "dashed")
  
  data <- data[ order(data$col, decreasing = FALSE), ]
  
  # Check zoomStart, zoomEnd
  if(is.null(xStart)) xStart <- min(c(data$xSNP, data$xGene), na.rm = TRUE)
  if(is.null(xEnd)) xEnd <- max(c(data$xSNP, data$xGene), na.rm = TRUE)
  xRange <- c(xStart, xEnd)
  
  # Plot --------------------------------------------------------------------  
  # if there is no eqtl then plot warning.
  if(nrow(data) == 0){
    gg_out <- plotBlank(xStart = xStart, xEnd = xEnd, yLabel = label, textAnnot = "No eQTL")
  } else {
    geneLabel <- unique(data[, c("xGene", "gene")])
    gg_out <- 
      ggplot(data = data,
             aes(x = xSNP, xend = xGene,
                 y = 1, yend = 0.35, col = col, linetype = linetype)) +
      geom_segment() +
      geom_text(aes(x = xGene, y = 0.35, label = gene),
                data = geneLabel, inherit.aes = FALSE, angle = 90,
                hjust = 1) +
      scale_color_identity() +
      scale_linetype_identity() +
      scale_y_continuous(name = "",
                         breaks = c(0.35),
                         labels = if(pad) strPadLeft(label) else label,
                         limits = c(-0.10, 1)) +
      coord_cartesian(xlim = xRange) +
      theme(axis.text.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.x = element_blank()) 
  }
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)
  
  # Output ------------------------------------------------------------------
  gg_out
} #END plotEQTL



