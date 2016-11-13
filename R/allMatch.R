#' Match all elements in vector.
#'
#' Return all matching element indexes in subject vector from query vector.
#' @param query a vector to look up in subject vector
#' @param subject vector for indexing of matched query
#' @return list object
#' @export allMatch
#' @author Daniel Leongamornlert
#' @examples
#' # Input query and subject vecots
#' match.list <- allMatch(query = c(1, 2, 3), subject = c(1, 1, 2, 2, 3, 3, 2))
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
    
    res <- lapply(query, function(i){
      which(i == subject) 
    })
    names(res) <- query
    
    return(res)
    
}

