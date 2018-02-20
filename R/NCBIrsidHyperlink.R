#' LocusExplorer ggplot custom theme
#'
#' LocusExplorer ggplot custom theme.
#' @keywords NCBI locusexplorer hyperlink
#' @author Tokhir Dadaev
#' @export NCBIrsidHyperlink

NCBIrsidHyperlink <- function(SNP,
                              link = "https://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=",
                              html = TRUE){
  
  if(html){
    ifelse(substr(SNP, 1, 2) == "rs",
           paste0('<a href="', link, gsub("rs", "", SNP, fixed = TRUE),
                  '" target="_blank">', SNP, '</a>'),
           SNP)
  } else {
    ifelse(substr(SNP, 1, 2) == "rs",
           paste0(link, gsub("rs", "", SNP, fixed = TRUE)),
           SNP)
  }
}
