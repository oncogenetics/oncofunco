#' Match all elements in vector.
#'
#' Return all matching indexes in subject vector from query vector.
#' @export allMatch
#' @examples
#' # Input query and subject vecots
#' match.list <- allMatch(query = c(1,2,3), subject = c(1,1,2,2,3,3,2))
#'
#' # Output is query length list with subject indexes
#' match.list
#'
#' # $`1`
#' # [1] 1 2
#' #
#' # $`2`
#' # [1] 3 4 7
#' #
#' # $`3`
#' # [1] 5 6

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
out <- lapply(1:nrow(result.mat), function(x) {result.mat[x,][! is.na(result.mat[x,])]})
names(out) <- query
return(out)
}
