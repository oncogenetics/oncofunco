#' Match all elements in vector.
#'
#' Return all matching element indexes in subject vector from query vector.
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

allMatch <-
  function(query, subject) {
    # first match
    m1 <- match(query, subject)
    mx <- m1
    # remove match
    subject[mx] <- NA
    # add to output
    result <- m1

    # while there is a single match
    while (sum(is.na(mx)) != length(mx)) {
      m2 <- match(query, subject)
      mx <- m2
      subject[mx] <- NA
      result <- c(result, m2)
    }

    # convert results to matrix
    result.mat <- matrix(result, nrow = length(query))

    # convert results to list
    out <-
      lapply(1:nrow(result.mat), function(x) {
        result.mat[x, ][!is.na(result.mat[x, ])]
      })
    # add names
    names(out) <- query
    return(out)
  }

