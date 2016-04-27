#' A formatted dataframe output for glm() object.
#'
#' This function allows you to get pretty GLM
#' @param model Output of glm() Defaults to NA.
#' @param description Description column, string, Defaults to NA.
#' @param summary condensed summary Defaults to FALSE.
#' @keywords glm summary output
#' @export
#' @examples
#' glmSummary()

glmSummary <- function(model = NA, description = NA, summary = FALSE) 
{ 
  #Get LCI, OR, UCI (lower Confidence, odds ratio, upper confidence)
  
  #get summary
  coeffs <- as.data.frame(coef(summary(model)))
  #c("Estimate","Std. Error","z value","Pr(>|z|)")
  #c("Estimate" ,"Std. Error","t value","Pr(>|t|)")
  
  coeffs$OR   <- round(exp(coeffs$Estimate),2) #Odds Ratio
  coeffs$Lo.CI<- round(exp(coeffs$Estimate - 1.96 * coeffs$"Std. Error"),2) #Lower Confidence Interval
  coeffs$Up.CI<- round(exp(coeffs$Estimate + 1.96 * coeffs$"Std. Error"),2) #Upper Confidence Interval
  
  coefColNames <- colnames(coeffs)
  colnames(coeffs) <- c("Est","SE",
                        #toupper(substr(coefColNames[grepl("value",coefColNames)],1,1)),
                        "TZ",
                        "P","OR","Lo.CI","Up.CI")
  
  coeffs[,c(1,2,3)] <- round(coeffs[,c(1,2,3)],2)
  coeffs$P <- format(coeffs$P,scientific=T)
  
  #output as data.frame
  if(summary)
  {
    result <- data.frame(
      Description=description,
      P=coeffs$P,
      OR=paste0(coeffs$OR," (", coeffs$Lo.CI,"-", coeffs$Up.CI,")"))
    colnames(result)[3] <- "OR (CI.95)"
    row.names(result)<-NULL
  } else 
  {
    result <- as.data.frame(
      cbind(description, coeffs),
      stringsAsFactors=F)
  }
  #if description is NA then exclude
  result <- if(is.na(description)){
    result[,-match("Description",colnames(result))]}else{
      result}
  
  return(result)
}