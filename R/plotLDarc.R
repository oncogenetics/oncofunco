#' LocusExplorer - LDarc plot
#'
#' LD Arc plot for LocusExplorer.
#' @param LD plink LD output format, data.frame object with c("BP_A","SNP_A","BP_B","SNP_B","R2") columns. Optional/recommended.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param upper,lower SNP names to label in the plot. Must be present in assoc data.frame.
#' @param statNames Two statistics methods name used for y-axis, string, default c("Stepwise", "JAM").
#' @param minR2 Filter LD on minimum R2, filter out "non-significant" relationships.
#' @param pad Default is TRUE, to align plots pad strings with spaces, using oncofunco::strPadLeft().
#' @param title character string for plot title. Default is NULL, i.e.: no plot title. 
#' @export plotLDarc
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords LD plot SNP genetics

plotLDarc <- function(LD = NULL,
                      xStart = NULL,
                      xEnd = NULL,
                      upper = NULL,
                      lower = NULL,
                      statNames = c("Stepwise", "JAM"),
                      minR2 = 0.4,
                      hitsOnly = FALSE,
                      pad = TRUE,
                      title = NULL){
  # Check input data --------------------------------------------------------
  if(is.null(LD)) stop("LD is missing, must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')")
  if(!all(c("BP_A","SNP_A","BP_B","SNP_B","R2") %in% colnames(LD))){
    stop("LD must have columns: c('BP_A','SNP_A','BP_B','SNP_B','R2')") 
  } else LD <- as.data.frame(LD)
  
  # upper lower hits
  if(is.null(upper) | is.null(lower)) stop("Hits for upper/lower cannot be empty, must match on LD$SNP_A.")
  if(length(intersect(upper, LD$SNP_A)) == 0 |
     length(intersect(lower, LD$SNP_A)) == 0) stop("Hits for upper/lower must match on LD$SNP_A.")
  
  if(is.null(statNames)) stop("Provide stats names for 2 methods, label for y-axis.")
  if(length(statNames) > 2) {
    statNames <- statNames[1:2]
    warning("statNames length is more than 2, first 2 will be used to label y-axis.")}
  
  # Data prep ---------------------------------------------------------------  
  hits <- unique(c(upper, lower))
  
  # subset on hits and R2 min
  datLD <- LD[ LD$R2 > minR2 &
                 LD$BP_A != LD$BP_B &
                 LD$SNP_A %in% hits, ]
  # show only relationship between hits of 2 methods.
  if(hitsOnly){
    datLD <- datLD[ datLD$SNP_A %in% hits &
                      datLD$SNP_B %in% hits, ]}
  
  # round to have less colour 
  datLD$R2 <- round(datLD$R2, 2)

  # Check zoomStart, zoomEnd
  if(is.null(xStart)) xStart <- min(c(datLD$BP_A, datLD$BP_B), na.rm = TRUE) - 10000
  if(is.null(xEnd)) xEnd <- max(c(datLD$BP_A, datLD$BP_B), na.rm = TRUE) + 10000
  xRange <- c(xStart, xEnd)
  
  
  upperDat <- datLD[ datLD$SNP_A %in% upper, ]
  upperDat$From <- pmax(upperDat$BP_A, upperDat$BP_B)
  upperDat$To <- pmin(upperDat$BP_A, upperDat$BP_B)
  upperDat <- upperDat[ order(upperDat$R2), ]
  
  lowerDat <- datLD[ datLD$SNP_A %in% lower, ]
  lowerDat$From <- pmin(lowerDat$BP_A, lowerDat$BP_B)
  lowerDat$To <- pmax(lowerDat$BP_A, lowerDat$BP_B)
  lowerDat <- lowerDat[ order(lowerDat$R2), ]
  
  plotDat <- rbind(upperDat, lowerDat)
  
  annotText <- unique(
    rbind(
      data.frame(
        SNP = substr(upperDat$SNP_A, 1, 15),
        BP = upperDat$BP_A,
        Y = 0.75,
        Yend = 0.5),
      data.frame(
        SNP = substr(lowerDat$SNP_A, 1, 15),
        BP = lowerDat$BP_A,
        Y = 0.25,
        Yend = 0.5)))
  annotText <- annotText[ order(annotText$Y, annotText$BP), ]

  annotText[ c(TRUE, FALSE), "Y"] <- annotText[ c(TRUE, FALSE), "Y"] - 0.05
  
  # Plot --------------------------------------------------------------------
  if(pad) statNames <- strPadLeft(statNames)
  
  gg_out <- 
    ggplot(plotDat, aes(x = From, xend = To, y = 0.5, yend = 0.5, col = R2)) +
    geom_segment(aes(x = BP, xend = BP,
                     y = Y, yend = Yend),
                 linetype = "dashed", col = "grey60",
                 data = annotText, inherit.aes = FALSE) +
    geom_curve(curvature = 1, ncp = 1000, lineend = 'butt') +
    geom_hline(yintercept = 0.5, col = "grey60") +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0.25, 0.75),
                       labels = c(statNames[2], statNames[1])) +
    geom_label_repel(aes(x = BP, y = Y, label = SNP), data = annotText,
                     inherit.aes = FALSE) +
    scale_color_gradient2(low = "grey90", mid = "yellow", high = "red",
                          limits = c(0, 1), midpoint = 0.5) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank())

  #Zoom
  gg_out <- gg_out + coord_cartesian(xlim = xRange)
  
  #add title
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)
  
  # Output ------------------------------------------------------------------
  gg_out
  }
