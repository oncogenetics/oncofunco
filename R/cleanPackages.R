#' Remove all non-default packages from session.
#'
#' Remove all non-default packages from session.
#' @param verbose print out summary, default is \code{TRUE}
#' @export cleanPackages
#' @author Daniel Leongamornlert
#' @keywords clean session package detach


cleanPackages <- function(verbose = TRUE) {
  #if no other packages then stop
  if(is.null(names(sessionInfo()$otherPkgs))){ stop("Nothing to detach.") }
  
  #list of non base packages
  pkgs <- paste0('package:', names(sessionInfo()$otherPkgs))
  
  for(i in pkgs){
    detach(i, character.only = TRUE, unload = TRUE)
    #output summary
    if(verbose){
      #check if package is detached.
      if(i %in% names(sessionInfo()$otherPkgs)){
        cat(paste(i, "Failed to detach!!!\n"))
      } else { cat(paste(i, "Detached\n")) }
    }
  }
}


