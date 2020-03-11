# Theme simple

#' @title Theme simple
#' @description  Simple plot theme
#'
#' @param base_size Base size
#' @param base_family Base font family
#'
#' @rdname theme_simple
#'
#' @keywords internal
#' @export 
#' @import ggplot2
#' @import extrafont
theme_simple <- function (base_size = 12, base_family = "Arial"){
  extrafont::loadfonts(quiet = TRUE)
  theme_bw(base_size = base_size) + theme(text = element_text(family = base_family, 
                                                              color = "grey20", size = base_size), strip.background = element_blank(), 
                                          strip.text = element_text(hjust = 0), panel.grid.major = element_blank(), 
                                          panel.grid.minor = element_line(colour = "grey70", size = 0.15), 
                                          panel.background = element_blank(), panel.border = element_blank(), 
                                          axis.line = element_line(color = "grey20", size = 0.25), 
                                          plot.margin = unit(rep(0.5, 4), "cm"), legend.position = "right", 
                                          legend.text = element_text(size = base_size * 1.2), plot.caption = element_text(hjust = 1, 
                                                                                                             size = base_size * 0.75, colour = "grey30"), 
                                          plot.subtitle = element_text(size = base_size, 
                                                                                                                                                                        colour = "grey40"), 
                                          plot.title = element_text(size = base_size * 1.5, 
                                                                                                                                                                                                                      face = "bold"))  
} 
