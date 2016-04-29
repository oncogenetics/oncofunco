#' LocusExplorer ggplot custom theme
#'
#' LocusExplorer ggplot custom theme.
#' @keywords ggplot theme locusexplorer
#' @import ggplot2
#' @export

ggThemeLE <- function(){
  # LocusExplorer ggplot custom theme
  # General options for all plots -------------------------------------------
  # Usage: ggplot() + ggThemeLE()
  
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60",
                                          linetype = "dotted"),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        # Y Axis font:
        #   This will be obsolete once we know how to align plots using
        #   maybe cowplot...
        axis.text.y = element_text(family="Courier",colour = "grey20"))
  }
