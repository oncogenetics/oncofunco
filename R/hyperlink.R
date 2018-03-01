#' LocusExplorer hyperlinks for SNPs and geneSymbols
#'
#' LocusExplorer ggplot custom theme.
#' @param id character vector of SNP rsids or geneSymbols, e.g.: "rs34948850" or "BOK"
#' @param type link type, "SNP" for SNPs and "geneSymbol" for genes. Default "SNP".
#' @param link Default, for type is SNP then NCBI link, type is geneSymbol then UCSC, or provide custom link.
#' @param html Default is TRUE, wrapped the link as HTML.
#' @keywords NCBI locusexplorer hyperlink
#' @author Tokhir Dadaev
#' @export hyperlink

hyperlink <- function(id = NULL,
                      type = "SNP",
                      link = NULL,
                      html = TRUE){
  
  if(is.null(id)){ stop("id is missing, please provide SNP rsids or gene symbols.")}
  
  if(is.null(link) & type == "SNP"){
             
    link <- "http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=" } else {
      if(is.null(link) & type == "geneSymbol"){
        link <- "http://genome-euro.ucsc.edu/cgi-bin/hgGene?db=hg19&hgg_gene="
      }}
  # SNP -----------------------------------------------------------------------
  if(type == "SNP"){
    if(html){
      ifelse(substr(id, 1, 2) == "rs",
             paste0('<a href="', link, gsub("rs", "", id, fixed = TRUE),
                    '" target="_blank">', id, '</a>'),
             id)
    } else {
      ifelse(substr(id, 1, 2) == "rs",
             paste0(link, gsub("rs", "", id, fixed = TRUE)),
             id)
    }
    
  } else {
    # geneSymbol --------------------------------------------------------------
    if(type == "geneSymbol"){
      if(html){
        paste0('<a href="', link, id, '" target="_blank">', id, '</a>')
      } else {
        paste0(link, id)
      }
      
    } else {
      stop("type must be 'SNP' or 'geneSymbol'")}}
  
}

