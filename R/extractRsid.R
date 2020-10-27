#' Extract rsid
#'
#' This function extracts rsid from variant id
#' @param x character vector of variant IDs
#' @param sepIn separator used in varinat IDs, default ":"
#' @param sepOut separator used in output varinat IDs, where there is no rsid, default ":"
#' @return a character vector of variant IDs
#' @keywords impute2 rsid extract SNP
#' @export extractRsid
#' @author Tokhir Dadaev
#' @examples
#' extractRsid(c("1:154:CTT:C", "rs79438587:1543:C:T", "rs7"))

extractRsid <- function(x, sepIn = ":", sepOut = ":"){
  unname(unlist(
    sapply(x, function(i){
      d <- unlist(strsplit(i, split = sepIn, fixed = TRUE))
      ifelse(grepl("^rs", d[1]), d[1], paste(d, collapse = sepOut))
    })))
}
