#' Calculate MAF
#'
#' This function calculates MAF for imputed SNP data in dosage format.
#' @param z matrix object, rows are samples, columns are SNPs, values range 0-2.
#' @param noCall missing value for genotype, defaults to 9.
#' @param flip default TRUE. If maf is more than 0.5, then flip 1-maf.
#' @param dosageMax default is 2 , for chr23 use 1.
#' @return a \code{matrix} object. First column is MAF (range 0-0.5), second column is 1 if the MAF is flipped, else 0.
#' @keywords maf dosage snp
#' @export getMAF
#' @author Tokhir Dadaev
#' @examples
#' # dummy SNP data, 25 samples, 4 SNPs
#' set.seed(123)
#' geno <- matrix(sample(c(0, 1, 2), 100, replace = TRUE), ncol = 4)
#' # calculate MAF, returns 2 column matrix
#' getMAF(geno)


getMAF <- function(z = NULL, NoCall = 9, flip = TRUE, dosageMax = 2){

  if(!is.matrix(z)){
    stop('class(z) must be "matrix", rows are samples, columns are SNPs, range 0-2.')
  }

  # convert missing genotype to NA
  is.NoCall <- which(z == NoCall)
  z[is.NoCall] <- NA

  # check if dosage range is 0 and dosageMax
  if(min(z, na.rm = TRUE) < 0 |
     max(z, na.rm = TRUE) > dosageMax){
    stop(paste0("z matrix values range must be 0-", dosageMax))
  }

  # MAF = mean divided by dosageMax, excluding no-calls (NAs)
  maf <- colMeans(z, na.rm = TRUE) / dosageMax

  #if flipped mark it
  if(flip){
    maf <- cbind(maf = ifelse(maf > 0.5, 1 - maf, maf),
                 flipped = ifelse(maf > 0.5, 1, 0))
  } else {
    maf <- cbind(maf = maf,
                 flipped = 0)
  }

  #return MAF matrix
  return(maf)
}
