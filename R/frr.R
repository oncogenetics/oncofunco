#' Calculate Familial Relative Risk (FRR)
#'
#' This function calculates FRR and Contribution of FRR percentage.
#' @param data is data.frame object with 3 columns, `c("snp", "maf", "est")`.
#' @param familialRisk Familial risk to first-degree relatives of cancer cases. Default is 2.
#' @return data.frame with FRR and ContributionFRR columns.
#' @keywords risk snp familial frr
#' @export frr
#' @author Tokhir Dadaev


frr <- function(data, familialRisk = 2){
  
  # check input
  if(missing(data)) stop("data is missing: data.frame with 3 columns, c('snp', 'maf', 'est')")
  if(!is.data.frame(data)) stop("data is not a data.frame: data.frame with 3 columns, c('snp', 'maf', 'est')")
  if(!(is.numeric(data[[2]]) |
       is.numeric(data[[3]]))) stop("data: maf, est must be numeric columns: data.frame with 3 columns, c('snp', 'maf', 'est')")

  snp <- data[[1]]
  maf <- data[[2]]
  est <- data[[3]]
  
  FRR <- (maf * exp(est)^2 + (1 - maf)) /
         (maf * exp(est)   + (1 - maf))^2
  ContributionFRR <- log(FRR)/(log(familialRisk)/100)
  
  return(data.frame(FRR, ContributionFRR))
  
  }

# dummy data
# df1 <- read.table(text = "snp	maf	est
# snp1	0.7535	0.03
# snp2	0.5353	0
# snp3	0.1615	NA
# snp4	0.01 0.5
# snp5	NA	0.07
# snp6	0.707	0.01
# snp7	NA	NA
# snp8	0.1365	-0.03
# snp9	0.1988	-0.07", header = TRUE)
# 
# testing
# frr()
# frr(x)
# frr(data.frame(x = "a", "1", "3"))
# frr(df1[1:3,])
# 
# frr(df1)
# 
