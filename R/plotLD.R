#' LocusExplorer - LD plot
#'
#' LD plot for LocusExplorer.
#' @param data plink LD output format, data.frame object with c("BP_A","SNP_A","BP_B","SNP_B","R2") columns.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param hits SNP names to label in the plot.
#' @param hitsName alternative SNP names to label in the plot. Default same as `hits`
#' @param hitsColour Default NULL, uses ggplot colours.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param title character string for plot title. Default is NULL, i.e.: no plot title.
#' @export plotLD
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords LD plot SNP genetics

plotLD <- function(
  data = NULL,
  xStart = NULL,
  xEnd = NULL,
  hits = NULL,
  hitsName = hits,
  hitsColour = NULL,
  hitsLabel = TRUE,
  pad = TRUE,
  title = NULL){

  # Check input data --------------------------------------------------------
  if(is.null(data)) stop("data is missing, must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
  if(!all(c("BP_A","SNP_A","BP_B","SNP_B","R2") %in% colnames(data))){
    stop("data must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
  } else data <- as.data.frame(data)

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

  if(length(hits) > 0 & nrow(data) > 0){
    if(!all(hits %in% unique(data$SNP_A))){
      warning(paste0("Some SNPs (",
                     paste(setdiff(hits, unique(data$SNP_A)), collapse = ","),
                     ") did not match to data."))}
    #order hits by BP
    #c("BP_A","SNP_A","BP_B","SNP_B","R2")
    data <- as.data.frame(data)
    data <- data[ order(data$BP_A), ]
    hits <- unique(data[data$SNP_A %in% hits, "SNP_A"])

    # plot LD per hit SNPs on seperate Yaxis 1,2,3, etc.
    #colours for hits and pallete for R2 shades
    if(is.null(hitsColour)){
      colourLD <- oncofunco::colourHue(length(hits))
    } else {
      colourLD <- hitsColour[ seq_along(hits) ]
    }
    colourLDPalette <- unlist(lapply(colourLD, function(i){
      colorRampPalette(c("grey95", i))(100)}))

    # LD with R2 shades per segment matching Manhattan plot colours
    plotDat <- data %>%
      filter(SNP_A %in% hits) %>%
      arrange(BP_A) %>% 
      transmute(
        x = BP_B,
        xend = BP_B,
        y = as.numeric(factor(SNP_A, levels = hits)),
        yend = y + 1,
        LDColIndex = if_else(round(R2, 2) == 0, 1, round(R2, 2) * 100),
        hitColIndex = as.numeric(factor(SNP_A, levels = hits)),
        hitCol = colourLD[hitColIndex],
        LDCol = colourLDPalette[(hitColIndex - 1) * 100 + LDColIndex])

    # Plot segments ---------------------------------------------------------
    gg_out <- ggplot(data = plotDat,
                     aes(x = x, xend = xend,
                         y = y, yend = yend,
                         colour = LDCol)) +
      geom_hline(yintercept = 1:length(hits) + 0.5,
                 linetype = "dotted", col = "grey60") +
      geom_segment() +
      scale_colour_identity() +
      coord_cartesian(xlim = xRange) +
      scale_y_continuous(breaks = (1:length(hits)) + 0.5,
                         labels = if(pad) strPadLeft(hits) else substr(hits, 1, 20),
                         #limits = c(0, length(hits) + 1),
                         name = expression(LD[])
                         #name = expression(R^2)
                         )
  } else gg_out <- plotBlank(xStart, xEnd, yLabel = "LD")

  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)

  # Output ------------------------------------------------------------------
  gg_out

}
