#' A formatted output for glm() object
#'
#' This function allows you to get pretty GLM summary table.
#' @param Model Output of \code{glm()}, defaults to \code{NA}.
#' @param Description Description column, string, defaults to \code{NA}.
#' @param summary condensed summary, defaults to \code{FALSE}.
#' @return a \code{data.frame} object
#' @keywords glm summary output
#' @export
#' @author Tokhir Dadaev
#' @examples
#' fit <- glm(mpg ~ cyl + wt, data = mtcars)
#' glmSummary(Model = fit)
#' glmSummary(Model = fit, Description = "testing")
#' glmSummary(Model = fit, Description = "testing", Summary = TRUE)


glmSummary <- function(Model = NA, Description = NA, Summary = FALSE)
{
  #Get LCI, OR, UCI (lower confidence, odds ratio, upper confidence)

  #Input validation, Model must be of glm class
  if (!identical(class(Model), c("glm","lm"))) {
    stop('class(Model) must be "glm"')
  }

  #get summary
  coeffs <- as.data.frame(coef(base::summary(Model)))
  #c("Estimate","Std. Error","z value","Pr(>|z|)")
  #c("Estimate" ,"Std. Error","t value","Pr(>|t|)")

  #Odds Ratio, Lower Confidence Interval, Upper Confidence Interval
  coeffs$OR    <- round(exp(coeffs$Estimate),2)
  coeffs$Lo.CI <- round(exp(coeffs$Estimate - 1.96 * coeffs$"Std. Error"), 2)
  coeffs$Up.CI <- round(exp(coeffs$Estimate + 1.96 * coeffs$"Std. Error"), 2)

  #Stats can be Tscore or Zscore, to keep it standard rename it to TZ.
  colnames(coeffs) <- c("Est","SE","TZ",
                        "P","OR","Lo.CI","Up.CI")

  coeffs[,c(1,2,3)] <- round(coeffs[, c(1, 2, 3)], 2)
  coeffs$P <- format(coeffs$P, scientific = TRUE)

  #output as data.frame
  if(Summary)
  {
    result <- data.frame(
      Var = rownames(coeffs),
      Description = Description,
      P = coeffs$P,
      OR = paste0(coeffs$OR, " (", coeffs$Lo.CI, "-", coeffs$Up.CI,")"))
    colnames(result)[4] <- "OR (CI.95)"
    row.names(result) <- NULL
  } else
  {
    result <- as.data.frame(
      cbind(Description = Description, coeffs),
      stringsAsFactors = FALSE)
  }
  #if Description is NA then exclude
  result <- if(is.na(Description)){
    result[,-match("Description", colnames(result))]} else {
      result}

  return(result)
}
