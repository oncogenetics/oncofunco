#' A formatted output for glm() object
#'
#' This function allows you to get pretty GLM summary table.
#' @param model Output of \code{glm()}, defaults to \code{NA}.
#' @param description Description column, string, defaults to \code{NA}.
#' @param summary condensed summary, defaults to \code{FALSE}.
#' @param decimal rounding decimals, defaul \code{2}.
#' @param scientificP Scientific notation for p-value, default \code{TRUE}.
#' @return a \code{data.frame} object
#' @keywords glm summary output
#' @export glmSummary
#' @author Tokhir Dadaev
#' @examples
#' fit <- glm(mpg ~ cyl + wt, data = mtcars)
#' glmSummary(Model = fit)
#' glmSummary(Model = fit, Description = "testing")
#' glmSummary(Model = fit, Description = "testing", Summary = TRUE)


glmSummary <- function(model = NA, description = NA, summary = FALSE,
                       decimal = 2, scientificP = TRUE)
{
  #Get LCI, OR, UCI (lower confidence, odds ratio, upper confidence)

  #Input validation, Model must be of glm class
  if (!identical(class(model), c("glm","lm"))) {
    stop('class(model) must be "glm"')
  }

  #get summary
  coeffs <- as.data.frame(coef(base::summary(model)))
  #c("Estimate","Std. Error","z value","Pr(>|z|)")
  #c("Estimate","Std. Error","t value","Pr(>|t|)")

  #Odds Ratio, Lower Confidence Interval, Upper Confidence Interval
  coeffs$OR    <- round(exp(coeffs$Estimate), decimal)
  coeffs$Lo.CI <- round(exp(coeffs$Estimate - 1.96 * coeffs$"Std. Error"), decimal)
  coeffs$Up.CI <- round(exp(coeffs$Estimate + 1.96 * coeffs$"Std. Error"), decimal)

  #Stats can be Tscore or Zscore, to keep it standard rename it to TZ.
  colnames(coeffs) <- c("Est","SE","TZ", "P","OR","Lo.CI","Up.CI")

  coeffs[,c(1,2,3)] <- round(coeffs[, c(1, 2, 3)], decimal)
  coeffs$P <- format(coeffs$P, scientific = scientificP)

  #output as data.frame
  if(summary)
  {
    result <- data.frame(
      Var = rownames(coeffs),
      Description = description,
      P = coeffs$P,
      OR = paste0(coeffs$OR, " (", coeffs$Lo.CI, "-", coeffs$Up.CI,")"))
    colnames(result)[4] <- "OR (CI.95)"
    row.names(result) <- NULL
  } else
  {
    result <- as.data.frame(
      cbind(Description = description, coeffs),
      stringsAsFactors = FALSE)
  }
  #if Description is NA then exclude
  result <- if(is.na(description)){
    result[, -match("Description", colnames(result))]} else {
      result}

  return(result)
}
