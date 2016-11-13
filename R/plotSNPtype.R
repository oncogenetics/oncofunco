#' LocusExplorer - SNP type plot
#'
#' SNP type plot for LocusExplorer.
#' @param assoc SNP association results, data.frame object with c("SNP","BP","P") columns. Required.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param title character string for plot title. Default is NULL, i.e.: no plot title. 
#' @param typedColName Column name in assoc data.frame to indicate typed/imputed SNPs, default "TYPED".
#' @param typedValue Value for typed SNPs, default is 2.
#' @param colTyped Colour for typed SNPs, default "#003333".
#' @param colImputed Colour for imputed SNPs, default "#669999".
#' @export plotSNPtype
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords plot SNP genetics

plotSNPtype <- function(
  assoc = NULL,
  xStart = NULL,
  xEnd = NULL,
  pad = TRUE,
  title = NULL,
  typedColName = "TYPED",
  typedValue = 2,
  colTyped = "#003333",
  colImputed = "#669999"){
  
  # Check input - assoc -------------------------------------------------------
  #check assoc
  if(is.null(assoc)) stop("assoc is missing, must provide assoc with columns: c('SNP','BP','P')")
  if(!all(c("SNP","BP","P") %in% colnames(assoc))) stop("assoc must have columns: c('SNP','BP','P')")
  # if SNP type is missing set all as typed
  if(!"TYPED" %in% colnames(assoc)){assoc$TYPED <- 2}
  assoc <- as.data.frame(assoc)
  
  # Check zoomStart, zoomEnd
  if(is.null(xStart)) xStart <- min(assoc$BP, na.rm = TRUE)
  if(is.null(xEnd)) xEnd <- max(assoc$BP, na.rm = TRUE)
  xRange <- c(xStart, xEnd)
  
  # data for plot
  plotDat <- unique(assoc[ , c("BP", typedColName)])
  colnames(plotDat)[2] <- "TYPED"
  #head(plotDat)
  
  # Plot segments ---------------------------------------------------------
  gg_out <-
    ggplot(data = plotDat,
           aes(x = BP, xend = BP, 
               y = TYPED - 1, yend = TYPED, 
               colour = ifelse(TYPED == typedValue, colTyped, colImputed))) +
    geom_segment() +
    geom_hline(yintercept = 0:1 + 0.5,
               linetype = "dotted", col = "grey60") +
    scale_color_identity() +
    coord_cartesian(xlim = xRange) + 
    scale_y_continuous(breaks = c(0.5, 1.5),
                       labels = if(pad) strPadLeft(c("Imputed", "Typed")) else c("Imputed", "Typed"),
                       #limits = c(-0.90, 2),
                       name = "SNP") +
    #xlab(NULL) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)
  
  # Output ------------------------------------------------------------------
  gg_out
}
