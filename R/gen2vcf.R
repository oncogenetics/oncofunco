#' Convert IMPUTE2 gen to VCFv4.1 format, input for fastqtl 
#'
#' This function converts IMPUTE2 gen file posterior probabilities to dosage, then outputs to VCF file.
#' @param genFile input gen file
#' @param sampleFile if provided sampleID will be added as header, else X1..Xn as sampleID
#' @param chrName chromosome name \code{c(1:22,X,Y)}
#' @param outVCFFile output file name for VCF
#' @param outVCFHeader output VCF header lines
#' @param subsetSNPs index of SNPs to subset - rows
#' @param subsetSamples index of Samples to subset - coloumns
#' @return outputs VCF file, nothing returned in the workspace
#' @keywords impute2 gen dose dosage convert vcf fastqtl
#' @author Tokhir Dadaev
#' @export gen2vcf

gen2vcf <- function(genFile, sampleFile, chrName,
                    outVCFFile, outVCFHeader = "##fileformat=VCFv4.1",
                    subsetSNPs = NULL, subsetSamples = NULL){
  # - convert to VCF format for http://fastqtl.sourceforge.net/
  
  # input: post probs file, output form IMPUTE2
  # gen file example
  # chr,var,bp,a1,a2,genotype=3 columns per sample, probs for AA, AB, BB
  # --- 1:10056107:G:C 10056107 G C 1 0 0
  # --- 1:10056108:G:A 10056108 G A 1 0 0
  # --- rs141860841:10056152:A:G 10056152 A G 1 0 0
  
  # output: 
  # ##fileformat=VCFv4.1
  # #CHROM	POS	ID	REF	ALT	QUAL	FILTER	INFO	FORMAT UNR1	UNR2	UNR3	UNR4
  # chr7	123	SNP1	A	G	100	PASS	INFO	GT:DS	0/0:0.001	0/0:0.000	0/1:0.999	1/1:1.999
  # chr7	456	SNP2	T	C	100	PASS	INFO	GT:DS	0/0:0.001	0/0:0.000	0/1:1.100	0/0:0.100
  # chr7	789	SNP3	A	T	100	PASS	INFO	GT:DS	1/1:2.000	0/1:1.001	0/0:0.010	0/1:0.890
  
  
  dose <- gen2dose(genFile = genFile, sampleFile = sampleFile,
                   chrName = chrName)
   dose <- gen2dose(genFile = "example.gen", sampleFile = "example.sample",
                    chrName = "22")
  
  d1 <- dose[, c("V3", "V2", "V4", "V5")]
  colnames(d1) <- c("POS", "ID", "REF", "ALT")
  d2 <- data.table::data.table(QUAL = 100,
                   FILTER = "PASS",
                   INFO = "INFO",
                   FORMAT = "DS")
  
  outVCF <- cbind(
    #VCF 9 fixed, mandatory columns.
    "#CHROM" = chrName, d1, d2,
    #VCF genotype as dosage
    dose[, 7:ncol(dose), with = FALSE])
  
  # Subset SNPs -------------------------------------------------------------
  if(!is.null(subsetSNPs)) {
    outVCF <- outVCF[ subsetSNPs, ]
  }
  
  # Subset Samples ----------------------------------------------------------
  if(!is.null(subsetSamples)) {
    outVCF <- outVCF[, subsetSamples + 9, with = FALSE]
  }
  
  # header VCF 
  write(outVCFHeader, file = outVCFFile)
  # write genotype table, append to existing file with a VCF header
  write.table(outVCF, file = outVCFFile,
              append = TRUE, quote = FALSE, sep = "\t", row.names = FALSE)
}
