#' Calculate MAF
#'
#' This function calculates MAF for imputed SNP data in dosage format.
#' @param Z matrix object, rows are samples, columns are SNPs, values range 0-2.
#' @param NoCall missing value for genotype, defaults to 9.
#' @return a \code{matrix} object. First column is MAF (range 0-0.5), second column is 1 if the MAF is flipped, else 0.
#' @keywords maf dosage snp
#' @export
#' @examples
#' # dummy SNP data, 25 samples, 4 SNPs
#' set.seed(123)
#' geno <- matrix(sample(c(0, 1, 2), 100, replace = TRUE), ncol = 4)
#' calculate MAF, returns 2 column matrix
#' getMAF(geno)


getMAF <- function(Z = NULL, NoCall = 9){

  if(!is.matrix(Z)){
    stop('class(Z) must be "matrix", rows are samples, columns are SNPs, range 0-2.')
  }

  # convert missing genotype to NA
  is.NoCall <- which(Z == NoCall)
  Z[is.NoCall] <- NA

  # check if dosage range is 0-2
  if(min(Z, na.rm = TRUE) < 0 |
     max(Z, na.rm = TRUE) > 2){
    stop('Z matrix values range must be 0-2')
  }

  # MAF = mean divided by 2, ecluding no-calls (NAs)
  maf <- colMeans(Z, na.rm = TRUE) / 2

  # if flipped mark it, MAF range can be between 0-0.5
  maf <- cbind(ifelse(maf > 0.5, 1 - maf, maf),
               ifelse(maf > 0.5, 1, 0))

  #return MAF matrix
  return(maf)
}
