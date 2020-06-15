#' LocusExplorer - LD plot matrix heatmap
#'
#' LD plot for LocusExplorer.
#' @param data plink LD output format, data.frame object with c("BP_A","SNP_A","BP_B","SNP_B","R2") columns.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param hits SNP names to label in the plot. Must be present in assoc data.frame.
#' @param hitsName Alternative SNP names to label in the plot. Default same as `hits`
#' @param hitsLabel Default is TRUE, set to FALSE not to show SNP names on the plot.
#' @param hitsNameLength Maximum characters for the label.
#' @param cols colours, length 6.
#' @param title Character string for plot title. Default is NULL, i.e.: no plot title.
#' @param legend Show plot legend, default TRUE.
#' @export plotLDmatrix
#' @import data.table
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords LD plot SNP genetics

plotLDmatrix <- function(
  data = NULL,
  xStart = NULL,
  xEnd = NULL,
  hits = NULL,
  hitsName = hits,
  hitsLabel = TRUE,
  hitsNameLength = 20,
  cols = c("lightblue", "#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000"),
  title = NULL,
  legend = TRUE){

  # Check input data --------------------------------------------------------
  if(is.null(data)) stop("data is missing, must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
  if(!all(c("BP_A","SNP_A","BP_B","SNP_B","R2") %in% colnames(data))){
    stop("data must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
  } else setDT(data)

  # Check zoomStart, zoomEnd
  if(is.null(xStart)) xStart <- min(data$BP_B, na.rm = TRUE)
  if(is.null(xEnd)) xEnd <- max(data$BP_B, na.rm = TRUE)
  xRange <- c(xStart, xEnd)

  if(is.null(hits)){
    hits <- unique(data$SNP_A)
    hits <- hits[1:min(5, length(hits))]
    warning(
      paste("hits missing, selected first <5 SNPs as hits from data$SNP_A, n = ",
            length(unique(data$SNP_A))))
  } else hits <- intersect(hits, data$SNP_A)

  if(length(hits) > 0){
    if(!all(hits %in% unique(data$SNP_A))){
      warning(paste0("Some SNPs (",
                     paste(setdiff(hits, unique(data$SNP_A)), collapse = ","),
                     ") did not match to data."))}

    #data <- fread("chr1_150158287_151158287_LD.txt")
    data <- data[ SNP_A %in% hits & SNP_B %in% hits, ]

    # get all combo
    plotDat <- data.table(expand.grid(unique(c(data$SNP_A, data$SNP_B)),
                                      unique(c(data$SNP_A, data$SNP_B))))
    setnames(plotDat, c("SNP_A", "SNP_B"))
    # add R2
    plotDat <- merge(plotDat, data[, .(SNP_A, SNP_B, R2)], 
                     by = c("SNP_A", "SNP_B"), all.x = TRUE)
    plotDat[ is.na(plotDat) ] <- 0

    # add pos for SNP_A and SNP_B
    plotDat <- merge(plotDat, unique(data.table(SNP_A = c(data$SNP_A, data$SNP_B),
                                                BP_A = c(data$BP_A, data$BP_B))),
                     by = "SNP_A")
    plotDat <- merge(plotDat, unique(data.table(SNP_B = c(data$SNP_A, data$SNP_B),
                                                BP_B = c(data$BP_A, data$BP_B))),
                     by = "SNP_B")

    # order SNPs by pos - as factor levels
    SNPorder <- unique(rbind(
      plotDat[, .(SNP = SNP_A, BP = BP_A)],
      plotDat[, .(SNP = SNP_B, BP = BP_B)]
    ))[ order(BP), SNP]
    
    plotDat[, SNP_A := factor(SNP_A, levels = SNPorder, 
                              labels = substr(SNPorder, 1, hitsNameLength)) ]
    plotDat[, SNP_B := factor(SNP_B, levels = SNPorder, 
                              labels = substr(SNPorder, 1, hitsNameLength)) ]

    # R2 cut colours
    #res$col <- as.character(cut(res$R2, seq(0, 1, 0.2), labels = grey.colors(5, 0.9, 0)))
    plotDat[, col := as.character(cut(R2, seq(0, 1, 0.2),
                                      labels = cols[ -1 ])) ]
    plotDat[R2 == 0, col := cols[ 1 ] ]

    # # output plot tile
    if(legend) {l = "legend"} else {l = "none"}
    
    gg_out <- ggplot(plotDat, aes(SNP_A, SNP_B, fill = col, col = "white")) +
      geom_tile() +
      scale_fill_identity(guide = l, name = expression(R^2),
                          breaks = rev(cols),
                          limits = rev(cols),
                          labels = rev(seq(0, 1, 0.2)), drop = FALSE) +
      scale_color_identity() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
            axis.title = element_blank(),
            legend.background = element_rect(colour = "grey90"))

  }else{ gg_out <- plotBlank(xStart, xEnd, yLabel = "LD") }
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)){ gg_out <- gg_out + ggtitle(title) }

  # Axis lables, SNP names --------------------------------------------------
  if(!hitsLabel){ gg_out <- gg_out + theme(axis.text.x = element_blank(),
                                           axis.text.y = element_blank()) }
  
  # Legend ------------------------------------------------------------------
  if(!legend){ gg_out <- gg_out + theme(legend.position = "none") }
  
  # Output ------------------------------------------------------------------
  #remove grids, return
  gg_out + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())

}
