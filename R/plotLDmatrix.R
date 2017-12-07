#' LocusExplorer - LD plot matrix heatmap
#'
#' LD plot for LocusExplorer.
#' @param data plink LD output format, data.frame object with c("BP_A","SNP_A","BP_B","SNP_B","R2") columns.
#' @param xStart,xEnd Region range, zoom, minimum BP and maximum BP, advised to keep this less than 5Mb.
#' @param hits SNP names to label in the plot. Must be present in assoc data.frame.
#' @param title character string for plot title. Default is NULL, i.e.: no plot title.
#' @export plotLDmatrix
#' @author Tokhir Dadaev
#' @return a \code{ggplot} object
#' @keywords LD plot SNP genetics

plotLDmatrix <- function(
  data = NULL,
  xStart = NULL,
  xEnd = NULL,
  hits = NULL,
  hitsLabel = TRUE,
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

  if(length(hits) > 0){
    if(!all(hits %in% unique(data$SNP_A))){
      warning(paste0("Some SNPs (",
                     paste(setdiff(hits, unique(data$SNP_A)), collapse = ","),
                     ") did not match to data."))}

    #data <- fread("chr1_150158287_151158287_LD.txt")
    data <- data %>% filter(SNP_A %in% hits & SNP_B %in% hits)

    # get all combo
    x <- data.frame(expand.grid(unique(c(data$SNP_A, data$SNP_B)),
                                unique(c(data$SNP_A, data$SNP_B))))
    colnames(x) <- c("SNP_A", "SNP_B")
    # add R2
    x <- merge(x, data[, c("SNP_A", "SNP_B", "R2")], by = c("SNP_A", "SNP_B"), all.x = TRUE)
    x[ is.na(x) ] <- 0

    # add pos for SNP_A and SNP_B
    res <- x
    d_bp <- merge(res, unique(data.frame(SNP_A = c(data$SNP_A, data$SNP_B),
                                         BP_A = c(data$BP_A, data$BP_B))),
                  by = "SNP_A")
    d_bp <- merge(d_bp, unique(data.frame(SNP_B = c(data$SNP_A, data$SNP_B),
                                          BP_B = c(data$BP_A, data$BP_B))),
                  by = "SNP_B")

    # order SNPs by pos - as factor levels
    SNPorder <-
      unique(
        rbind(
          data_frame(SNP = d_bp$SNP_A, BP = d_bp$BP_A),
          data_frame(SNP = d_bp$SNP_B, BP = d_bp$BP_B))
      ) %>% arrange(BP) %>% .$SNP

    res$SNP_A <- factor(res$SNP_A, levels = SNPorder)
    res$SNP_B <- factor(res$SNP_B, levels = SNPorder)

    # R2 cut colours
    #res$col <- as.character(cut(res$R2, seq(0, 1, 0.2), labels = grey.colors(5, 0.9, 0)))
    res$col <- as.character(cut(res$R2, seq(0, 1, 0.2),
                                labels = c("#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000")))
    res$col <- if_else(res$R2 == 0, "lightblue", res$col)

    # # output plot tile
    gg_out <- ggplot(res, aes(SNP_A, SNP_B, fill = col, col = "grey90")) +
      geom_tile() +
      scale_fill_identity(guide = "legend", name = expression(R^2),
                          breaks = rev(c("lightblue","#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000")),
                          limits = rev(c("lightblue","#fef0d9","#fdcc8a","#fc8d59","#e34a33","#b30000")),
                          labels = rev(seq(0, 1, 0.2)), drop = FALSE) +
      scale_color_identity() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
            axis.title = element_blank(),
            #legend.position = c(0.9, 0.4),
            legend.background = element_rect(colour = "grey90"))

  } else gg_out <- plotBlank(xStart, xEnd, yLabel = "LD")

  # Add title ---------------------------------------------------------------
  if(!is.null(title)) gg_out <- gg_out + ggtitle(title)

  # Output ------------------------------------------------------------------
  gg_out

}
