# Produce the ggplot theme that I like
#' myTheme
#'
#' This function returns a theme that can be added to a ggplot object
#' It is based on the theme_classic(), with no white background so that
#' if the plot is saved as pdf, it incorporates better with slides. Also it avoids
#' doing theme_smth() + theme(other options)
#' @param ... Other arguments passed to the \link{theme} function in ggplot2.
#' @return a theme
#' @import ggplot2
#' @export
my_theme <- function(...){
  theme_classic() +
    theme(rect = element_blank(),
          ...)
}
