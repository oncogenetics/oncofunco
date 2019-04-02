#' Miami plot
#'
#' Miami plot - WORK IN PROGRESS...
#' @param dat SNP association results, data.frame object with c("SNP","BP","P") columns. Required.
#' @param hits SNP names to label in the plot. Must be present in assoc data.frame.
#' @param hitsName alternative SNP names to label in the plot. Default same as `hits`
#' @param hitsLabel Default is TRUE, set to FALSE not to show SNP names on the plot.
#' @param hitsColour Default NULL, uses ggplot colours.
#' @param yMin,yMax y-axis range limits.
#' @param yRange 
#' @param yRangeBy y-axis ticks, setting to 5 means ticks will be placed at `c(0, 5, 10, ...)`.
#' @param title character string for plot title. Default is NULL, i.e.: no plot title.
#' @export plotMiami
#' @author Tokhir Dadaev, Edward J. Saunders
#' @return a \code{ggplot} object
#' @keywords manhattan miami plot SNP genetics

plotMiami <- function(
  dat = NULL,
  hits = NULL, 
  hitsName = hits,
  hitsLabel = TRUE,
  hitsColour = NULL,
  pMinFilter = 0.05, # pMinFilter = 0.0005
  pMaxCap = 1/10^50,
  yRange = c(-50, 50),
  yRangeBy = 5,
  colP1 = c("green", "limegreen"),
  colP2 = c("lightblue", "blue"),
  title = c("My Main Title", "My Top Title", "My Bottom Title")){
  #check if ggplot2, data.table available
  if (!requireNamespace(c("ggplot2", "data.table", "ggrepel"), quietly = TRUE)) {
    stop("Package data.table, ggplot2 packages needed for this function to work. Please install it.",
         call. = FALSE)
  } 
  # todo: need to agree on column names and format...
  #       for now manual check, to match plotManhattan input format.
  # filter OR cap
  dat <-
    rbind(
      dat[ overall_P.value < pMinFilter, 
           list(CHR = Chr, BP = Position, SNP = MarkerName,
                PLog10 = ifelse(overall_P.value <= pMaxCap, -log10(pMaxCap), -log10(overall_P.value)),
                col = colP1[(Chr %% 2) + 1])],
      dat[ adv_P.value < pMinFilter,
           list(CHR = Chr, BP = Position, SNP = MarkerName,
                PLog10 = ifelse(adv_P.value <= pMaxCap, log10(pMaxCap), log10(adv_P.value)),
                col = colP2[(Chr %% 2) + 1])]
    )
  
  # ordey by chr, bp
  dat <- dat[ order(CHR, BP), ]
  dat[ , x := .I ]
  dat[ , labelCHR := ifelse(rowidv(dat, cols = c("CHR")) == 1, CHR, NA)]
  
  # Base plot ---------------------------------------------------------------
  gg_out <- ggplot(dat, aes(x = x, y = PLog10, col = col)) +
    geom_point() +
    geom_text(aes(x = x, y = 0, label = labelCHR), col = "black", na.rm = TRUE)
  
  # Mark Hits: Labels -------------------------------------------------------
  # SNP names ro Gene mames on the plot for hits
  # if alternative names given then use those, hitsName
  
  if(!is.null(hitsLabel))
    if(hitsLabel){
      plotDat <- dat[ SNP %in% hits, ]
      plotDat$rn <- 1:nrow(plotDat)
      
      if(all(hits == hitsName)) {
        plotDat$label <- plotDat$SNP
      } else {
        x1 <- data.table(SNP = hits, label = hitsName, stringsAsFactors = FALSE)
        plotDat <- merge(plotDat, x1, by = "SNP")
        plotDat <- plotDat[ order(plotDat$rn), ]
      }
      gg_out <-
        gg_out +
        geom_text_repel(
          aes(x, PLog10, label = label), col = "black",
          data = plotDat)
      
      
    }
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(label = title[ 1 ],
                                                 subtitle = title[ 2 ])
  
  # General options ---------------------------------------------------------
  gg_out <- gg_out +
    scale_x_continuous(name = title[ 3 ], labels = NULL) +
    scale_y_continuous(
      breaks = seq(yRange[1], yRange[2], yRangeBy),
      labels = abs(seq(yRange[1], yRange[2], yRangeBy)),
      name = expression(-log[10](italic(p)))) +
    scale_colour_identity() +
    theme_minimal()
  
  # Output ------------------------------------------------------------------
  gg_out
}

# Testing -----------------------------------------------------------------
# x <- fread("miami_plot_input_overall_agg_raw_Pvalues.txt")
# plotMiami(dat = x,
#           hitsLabel = TRUE,
#           hits = c("8_128531689_A_T", "17_36099952_T_A", "23_66302821_A_G"),
#           hitsName = c("geneX\np=MyPvalue1", "geneB\np=MyPvalue2", "geneZ\np=MyPvalue1"),
#           pMinFilter = 0.0005,
#           pMaxCap = 1/10^50,
#           yRange = c(-50, 50),
#           yRangeBy = 10,
#           colP1 = c("green", "limegreen"),
#           colP2 = c("lightblue", "blue"),
#           title = c("My Main Title", "My Top Title", "My Bottom Title")
# )
