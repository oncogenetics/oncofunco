#' Convert IMPUTE2 gen to dose
#'
#' This function converts IMPUTE2 gen file posterior probabilities to dosage: collapse every 3 genetype columns for each sample into one columns as: 0*AA + 1*AB + 2*BB 
#' @param genFile input gen file
#' @param sampleFile if provided sampleID will be added as header, else X1..Xn as sampleID
#' @param chrName chromosome name \code{c(1:22,X,Y)}
#' @return a \code{data.frame} object
#' @keywords impute2 gen dose dosage convert
#' @export gen2dose

gen2dose <- function(genFile, sampleFile, chrName){
  # input: post probs file, output form IMPUTE2
  # gen file example
  # chr,var,bp,a1,a2,genotype=3 columns per sample, probs for AA, AB, BB
  # --- 1:10056107:G:C 10056107 G C 1 0 0
  # --- 1:10056108:G:A 10056108 G A 1 0 0
  # --- rs141860841:10056152:A:G 10056152 A G 1 0 0
  
  # output: dosage:
  # collapse every 3 gen columns for each sample into 1 as:
  #  0*AA + 1*AB + 2*BB 
  
  gen <- data.table::fread(genFile, data.table = FALSE)
  if(missing(sampleFile)){
    # if sample file is missing, then generate sampleIDs, gen file
    # has 5 leading MAP columns, then 3 columns per sample IDs.
    sampleID <- make.names(seq((ncol(gen)-5)/3))
  } else {
    sampleID <- data.table::fread(sampleFile, skip = 2, select = 1)[[ 1 ]]
  }
  
  
  MAP <- cbind(chrName, gen[, 1:5, with = FALSE ])
  
  AB <- gen[, seq(7, ncol(gen), 3), with = FALSE]
  BB <- gen[, seq(8, ncol(gen), 3), with = FALSE] * 2
  AB_BB <- AB + BB
  colnames(AB_BB) <- sampleID
  
  
  outDOSE <- cbind(MAP, AB_BB)
  
  return(outDOSE)
}
