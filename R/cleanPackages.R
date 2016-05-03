#' Remove all non-default packages from session.
#'
#' Remove all non-default packages from session.
#' @export cleanPackages


cleanPackages <- function() {
  pkgs = names(sessionInfo()$otherPkgs)
  pkgs = paste('package:', pkgs, sep = "")
  lapply(pkgs, detach, character.only = TRUE, unload = TRUE)
}
