#' Match all elements in vector.
#'
#' Return all matching indexes in query vector in subject vector.
#' @export allMatch

allMatch <- function(query, subject) {
m1 <- match(query, subject)
mx <- m1
result <- m1

while (sum(is.na(mx))!=length(mx)) {
  subject[mx] <- NA
  m2 <- match(query, subject)
  mx <- m2
  if(sum(is.na(mx))!=length(mx)) {
    result <- c(result, m2)
  }
}
result.mat <- matrix(result, nrow = length(query))
out <- lapply(1:dim(result.mat)[1], function(x) {result.mat[x,]})
names(out) <- query
return(out)
}
