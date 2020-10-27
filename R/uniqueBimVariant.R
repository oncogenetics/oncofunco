#' Fix Plink BIM file variant IDs when missing or duplicated
#'
#' This function calculates MAF for imputed SNP data in dosage format.
#' Related Biostars post: \url{https://www.biostars.org/p/315219/}
#' BIM file description: \url{https://www.cog-genomics.org/plink2/formats#bim}
#' @param fileIn Plink BIM filename
#' @param fileOut Output filename Plink BIM format
#' @param sep Separator to use when variant ID is missing, default "_".
#' @return none, outputs BIM file
#' @keywords plink bim variant snp convert
#' @export uniqueBimVariant
#' @author Tokhir Dadaev

uniqueBimVariant <- function(fileIn = NULL, fileOut = NULL, sep = "_"){
  
  # TODO check inputs, check if out file exists, warn overwrite, exit
  
  bim <- fread(fileIn, header = FALSE, 
               col.names = c("chr", "snp", "cm", "bp", "a1", "a2"))
  # https://www.cog-genomics.org/plink2/formats#bim
  # 1. Chromosome code (either an integer, or 'X'/'Y'/'XY'/'MT'; '0' indicates unknown) or name
  # 2. Variant identifier
  # 3. Position in morgans or centimorgans (safe to use dummy value of '0')
  # 4. Base-pair coordinate (1-based; limited to 231-2)
  # 5. Allele 1 (corresponding to clear bits in .bed; usually minor)
  # 6. Allele 2 (corresponding to set bits in .bed; usually major)

  # make unique names
  bim[ , snp := make.unique(
    ifelse(snp == ".", paste(chr, bp, a1, a2, sep = sep), snp)) ] 
  
  # output
  fwrite(bim, fileOut, sep = "\t", col.names = FALSE)
}
