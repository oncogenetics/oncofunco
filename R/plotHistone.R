#' LocusExplorer - ENCODE histone track plot
#'
#' ENCODE Histone plot for LocusExplorer.
#' H3K27Ac Mark (Often Found Near Active Regulatory Elements) on 7 cell lines from ENCODE 
#' http://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeRegMarkH3k27ac/
#' 
#' @param folder folder path for bigWig files
#' @param file bigWig file(s) names, defaults to encode filenames.
#' @param name defaults to encode names: c("GM12878","H1-hESC","HSMM","HUVEC","K562","NHEK","NHLF")
#' @param colour colour for bigWig files, defaults to encode colours: c("#FF1F1F","#FFB41F","#2ADFAF","#1FB4FF","#1F1FFF","#B41FFF","#FF1FB4")
#' @param alpha transparency for overlayed tracks, default 0.7.
#' @param chr,xStart,xEnd subset bigWig, chromosome number, postion start and end.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param title character string for plot title. Default is NULL, i.e.: no plot title. 
#' @export plotHistone
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords ENCODE bigWig plot histone

plotHistone <- function(
    folder = NULL,
    Type = "H3k27ac",
    name = c("GM12878","H1-hESC","HSMM","HUVEC","K562","NHEK","NHLF"),
    colour = c("#FF1F1F","#FFB41F","#2ADFAF","#1FB4FF","#1F1FFF","#B41FFF","#FF1FB4"),
    alpha = 0.7,
    chr = NULL,
    xStart = NULL,
    xEnd = NULL,
    pad = TRUE,
    title = NULL){
  # Check input data --------------------------------------------------------
  if(is.null(folder)) stop("Folder to bigWig files missing.")
  if(is.null(file)) stop("File name(s) to bigWig missing.")
  if(is.null(chr) | is.null(xStart) | is.null(xEnd)) stop("chr, xStart, xEnd missing.")
  if(!chr %in% paste0("chr", c(1:22, "X"))) stop("chr must be 'chr1', 'chr2', ... , 'chr22', 'chrX'")
  
  
  if (Type == "H3k27ac") {
    file = c("wgEncodeBroadHistoneGm12878H3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneH1hescH3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneHsmmH3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneHuvecH3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneK562H3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneNhekH3k27acStdSig.bigWig",
             "wgEncodeBroadHistoneNhlfH3k27acStdSig.bigWig")
  } else if (Type == "H3k4me1") {
    file = c("wgEncodeBroadHistoneGm12878H3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneH1hescH3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneHsmmH3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneHuvecH3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneK562H3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneNhekH3k4me1StdSig.bigWig",
             "wgEncodeBroadHistoneNhlfH3k4me1StdSig.bigWig")
  } else {
    file = c("wgEncodeBroadHistoneGm12878H3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneH1hescH3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneHsmmH3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneHuvecH3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneK562H3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneNhekH3k4me3StdSig.bigWig",
             "wgEncodeBroadHistoneNhlfH3k4me3StdSig.bigWig")
  }
  
  
  # to subset bigwig granges object
  gr <- GRanges(seqnames = chr, 
                IRanges(start = xStart,
                        end = xEnd))
  
  # Check input data bigWig exists --------------------------------------------
  files <- paste0(folder, file)
  if(!all(file.exists(files))) stop("Some bigWig files do not exist, check folder and file arguments.")
  
  plotDat <- try({
    rbindlist(
      #seven bigwig files
      lapply(seq(files), function(i){
        #i = 5
        x <- as.data.frame(rtracklayer::import(files[i], which = gr))
        x <- x[ x$score > 5, c("start", "end", "score")]
        x$score <- round(ifelse(x$score >= 100, 100, x$score),0)
        if(nrow(x) == 0) {x <- NULL} else {x$name <- name[i]}
        #return
        x
      }))
  })
  
  # Plot ----------------------------------------------------------------------
  if(inherits(plotDat, "try-error")){
    gg_out <- plotBlank(xStart = xStart, xEnd = xEnd)
  } else {
    gg_out <- 
      ggplot(plotDat, aes(xmin = start, xmax = end,
                          ymin = 0, ymax = score,
                          fill = name)) +
      geom_rect(alpha = alpha) +
      scale_fill_manual(values = colour) +
      scale_y_continuous(breaks = 50,
                         labels = if(pad) strPadLeft("Histone") else "Histone",
                         limits = c(0, 100),
                         name = expression(ENCODE[])) 
  }
  
  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)
  
  # Output ------------------------------------------------------------------
  gg_out
}
