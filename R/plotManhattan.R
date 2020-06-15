#' LocusExplorer - Manhattan plot
#'
#' Manhattan plot for LocusExplorer.
#' @param assoc SNP association results, data.frame object with c("SNP","BP","P") columns. Required.
#' @param LD plink LD output format, data.frame object with c("BP_A","SNP_A","BP_B","SNP_B","R2") columns. Optional/recommended.
#' @param geneticMap Recombination map, data.frame object with c("BP", "RECOMB") columns. Subset of one of genetic_map_*_combined_b37.txt, at http://mathgen.stats.ox.ac.uk/impute/1000GP_Phase3/ . Optional.
#' @param suggestiveLine Suggestive line, default is 5.
#' @param genomewideLine Genomewide link, dafault is 8.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param hits SNP names to label in the plot. Must be present in assoc data.frame.
#' @param hitsName alternative SNP names to label in the plot. Default same as `hits`
#' @param hitsLabel Default is TRUE, set to FALSE not to show SNP names on the plot.
#' @param hitsColour Default NULL, uses ggplot colours.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param postprob Default is FALSE, used for LocusExplorer to plot JAM PostProbs instead of Pvalues.
#' @param yRangeBy y-axis ticks, setting to 5 means ticks will be placed at `c(0, 5, 10, ...)`.
#' @param title character string for plot title. Default is NULL, i.e.: no plot title.
#' @param opts Default is c("Recombination","LD","LDSmooth","SuggestiveLine","GenomewideLine","Hits"), parts of plot to display.
#' @export plotManhattan
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords manhattan plot SNP genetics


plotManhattan <- function(
  assoc = NULL,
  LD = NULL,
  geneticMap = NULL,
  suggestiveLine = 5,
  genomewideLine = 8,
  xStart = NULL, 
  xEnd = NULL, 
  hits = NULL,
  hitsName = hits,
  hitsLabel = TRUE,
  hitsColour = NULL,
  pad = TRUE,
  postprob = FALSE,
  yRangeBy = NULL,
  title = NULL,
  opts = c("Recombination","LD","LDSmooth","SuggestiveLine",
           "GenomewideLine","Hits")){
  
  # Check input - assoc -------------------------------------------------------
  #check assoc
  if(is.null(assoc)) stop("assoc is missing, must provide assoc with columns: c('SNP','BP','P')")
  if(!all(c("SNP","BP","P") %in% colnames(assoc))) stop("assoc must have columns: c('SNP','BP','P')")
  # if SNP type is missing set all as typed
  if(!"TYPED" %in% colnames(assoc)){assoc$TYPED <- 2}
  assoc <- setDT(assoc, key = "BP")
  #set plog max
  assoc[, PLog := -log10(P) ]
  
  # XY range ------------------------------------------------------------------
  if(is.null(xStart)) xStart <- min(assoc$BP, na.rm = TRUE)
  if(is.null(xEnd)) xEnd <- max(assoc$BP, na.rm = TRUE)
  yMax <- ceiling(max(c(10, assoc$PLog)))
  if(is.null(yRangeBy)) yRangeBy <- ifelse(yMax >= 90, 10, 5)
  yRange <- c(0, max(c(10, ceiling((yMax + 1)/yRangeBy) * yRangeBy)))
  xRange <- c(xStart, xEnd)
  
  # If Y is post prob then update, range 0-1
  if(postprob){
    assoc$PLog <- assoc$P
    yMax <- 1
    yRangeBy <- 0.25
    yRange <- c(0, 1)
  }
  
  #Check input - recomb -------------------------------------------------------
  if("Recombination" %in% opts){
    if(is.null(geneticMap)) stop("geneticMap data is missing for recombination, must have columns: c('BP', 'RECOMB')")
    if(!all(c("BP", "RECOMB") %in% colnames(geneticMap))) stop("geneticMap data must have columns: c('BP', 'RECOMB')")
    geneticMap[, RECOMB_ADJ := RECOMB * yMax / 100 ]}
  
  # Plot all SNPs - background ------------------------------------------------
  gg_out <-
    ggplot(assoc, aes(x = BP, y = PLog)) +
    # all snps grey hollow shapes
    geom_point(size = 4, colour = "#B8B8B8", shape = assoc$TYPED, na.rm = TRUE) +
    geom_hline(yintercept = seq(0, yMax, yRangeBy),
               linetype = "dotted", col = "grey60")
  
  # Plot - Effect -----------------------------------------------------------
  if("Effect" %in% opts & "EFFECT" %in% colnames(assoc)){
    y_loess = predict(loess(EFFECT ~ BP, data = assoc, span = 0.1))
    y_loess_adj = scales::rescale(y_loess, to = c(yRange[2]/4 * 3, yRange[2]))
    vline = c(0, diff(sign(y_loess))) != 0
    datEffect <- data.table(
      BP = assoc$BP,
      log_OR = assoc$EFFECT,
      y_loess, y_loess_adj, vline)

    datEffect_shade <- datEffect[
      vline,
      .(xStart = BP,
        xEnd = data.table::shift(BP, type = "lead"),
        yStart = 0, yEnd = yRange[ 2 ],
        fill = rep_len(c("#996777", "#c5a8b1"), 
                       length.out = sum(vline)))][ !is.na(xEnd), ] 
    
    gg_out <- gg_out +
      geom_line(data = datEffect, aes(BP, y_loess_adj), col = "#6E273D") +
      geom_rect(data = datEffect_shade,
                aes(xmin = xStart, xmax = xEnd, ymin = yStart, ymax = yEnd,
                    fill = fill, alpha = 0.5),
                inherit.aes = FALSE, alpha = 0.2) +
      scale_fill_identity()
  }

  # Plot - Recombination ------------------------------------------------------
  if("Recombination" %in% opts & nrow(geneticMap) > 2 ){
    gg_out <- gg_out +
      geom_area(data = geneticMap,
                aes(BP, RECOMB_ADJ),
                fill = "#11d0ff", colour = "#00B4E0", alpha = 0.3)}

  # Check input - LD ----------------------------------------------------------
  if("LD" %in% opts | "LDSmooth" %in% opts){
    if(is.null(LD)) stop("LD is missing, must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
    if(!all(c("BP_A","SNP_A","BP_B","SNP_B","R2") %in% colnames(LD)))
      stop("LD must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
    
    LD <- setDT(LD)
    
    if(is.null(hits)){
      hits <- unique(LD$SNP_A)
      hits <- hits[1:min(5, length(hits))]
      warning(
        paste("hits missing, selected first <5 SNPs as hits from LD$SNP_A, n = :",
              length(unique(LD$SNP_A))))
    }
    
    if(is.null(hitsColour)){
      colourLD <- oncofunco::colourHue(length(hits))
    } else {
      colourLD <- hitsColour#[ seq_along(hits) ]
      }
    colourLDPalette <- unlist(lapply(colourLD, function(i){
      colorRampPalette(c("grey95", i))(100)}))
    
    #merge LD with assoc, to get R2 shades per point
    plotDat <- merge(
      LD[ SNP_A %in% hits, .(BP_A, SNP_A, BP_B, SNP_B, R2)],
      assoc[, .(BP, TYPED, PLog)],
      by.x = "BP_B", by.y = "BP", all = TRUE)[order(BP_A), ]
    plotDat[, LDColIndex := ifelse(round(R2, 2) == 0, 1, round(R2, 2) * 100)]
    plotDat[, hitColIndex := as.numeric(factor(SNP_A, levels = hits))]
    plotDat[, hitCol := colourLD[hitColIndex] ]
    plotDat[, LDCol := colourLDPalette[(hitColIndex - 1) * 100 + LDColIndex] ]
    plotDat[, R2Adj := yMax * R2 * 0.8]
      
    # Plot - LD Fill & LD Smooth --------------------------------------------
    #LD fill
    if("LD" %in% opts){
      gg_out <- gg_out +
        geom_point(data = plotDat, aes(BP_B, PLog),
                   size = 4,
                   shape = plotDat$TYPED + 15,
                   col = plotDat$LDCol,
                   alpha = 0.8, na.rm = TRUE, show.legend = FALSE)
      }
    #LDSmooth
    if("LDSmooth" %in% opts){
      gg_out <- gg_out +
        geom_smooth(data = plotDat, aes(x = BP_B, y = R2Adj, col = hitCol),
                    method = ifelse(nrow(plotDat) <=10, "lm", "loess"),
                    se = FALSE, na.rm = TRUE, 
                    formula = "y ~ x",
                    show.legend = FALSE)
    }
    
  } # END if("LD" %in% opts | "LDSmooth" %in% opts)
  
  
  # Suggestiveline ----------------------------------------------------------
  if("SuggestiveLine" %in% opts &
     !is.null(suggestiveLine) &
     !is.na(suggestiveLine) &
     is.numeric(suggestiveLine) &
     suggestiveLine > 0){
    gg_out <- gg_out +
      geom_hline(aes(yintercept = y), data = data.frame(y = suggestiveLine),
                 size = 0.5,
                 colour = "#1a9641")}
  # Genomewideline ----------------------------------------------------------
  if("GenomewideLine" %in% opts &
     !is.null(genomewideLine) &
     !is.na(genomewideLine) &
     is.numeric(genomewideLine) &
     genomewideLine > 0){
    gg_out <- gg_out +
      geom_hline(aes(yintercept = y), data = data.frame(y = genomewideLine),
                 size = 0.5,
                 colour = "#ca0020")}
  
  # Mark Hits: shape and vline ----------------------------------------------
  if("Hits" %in% opts & !is.null(hits) & any(hits %in% assoc$SNP)){
    gg_out <- gg_out +
      #mark hit SNPs - outline shapes
      geom_point(data = assoc[ SNP %in% hits, ],
                 aes(x = BP, y = PLog, shape = TYPED),
                 size = 4, colour = "black", na.rm = TRUE) +
      scale_shape_identity() +
      #mark hit SNPs - vertical lines
      geom_segment(data = assoc[ SNP %in% hits, ],
                   aes(x = BP, y = 0, xend = BP, yend = PLog),
                   colour = "black",
                   linetype = "dashed")}
  
  # Mark Hits: Labels -------------------------------------------------------
  # SNP names on the plot for hits,
  # if alternative names given then use those, hitsName
  if("Hits" %in% opts & length(hits) > 0)
    if(!is.null(hitsLabel))
      if(hitsLabel){
        plotDat <- assoc[ SNP %in% hits, ]
        plotDat[, label := setNames(hitsName, hits)[ hits ] ]

        gg_out <-
          gg_out +
          geom_text_repel(
            aes(BP, PLog, label = label),
            data = plotDat)
      }
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)
  
  # General options ---------------------------------------------------------
  gg_out <- gg_out +
    coord_cartesian(
      xlim = xRange,
      ylim = yRange) +
    scale_y_continuous(
      breaks = seq(0, yRange[2], yRangeBy),
      #labels = oncofunco::strPadLeft(seq(0, ROIPLogMax, 5)),
      labels = if(pad){strPadLeft(seq(0, yRange[2], yRangeBy))} else {
        seq(0, yRange[2], yRangeBy)},
      name = if(postprob){
        expression(PostProb[])
        } else {expression(-log[10](italic(p)))}
      ) +
    scale_colour_identity()
  
  # Output ------------------------------------------------------------------
  return(gg_out)
} #END plotManhattan
