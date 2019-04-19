# Produce the ggplot theme that I like
#' myTheme
#'
#' This function returns a theme that can be added to a ggplot object
#' It is based on the theme_classic, with some small tweaks.
#' @param ... Other arguments passed to the theme function in ggplot2.
#' @return a theme
#' @import ggplot2
#' @export
#' @details
#' This theme has two goals. First it makes the default behaviors that I like in
#' a plot: the classic theme, which is a nice ratio between purity and
#' annotations, no background color so that, when the image is saved as a pdf,
#' it incorporates better with slides or report, and a centered title when there
#' is one.
#' Secondly, it avoids doing theme_smth() + theme(other options) so that we can
#' do both in the same time. Drawback is that there is no auto-complete in that
#' case so that option should only be used for people already familiar with the
#' ggplot themes.
#' @examples
#' p <- ggplot2::ggplot(data.frame(x = 0:10, y = 0:10 + rnorm(11)),
#'             ggplot2::aes(x = x, y = y)) +
#'     ggplot2::geom_point()
#' p
#' p + DailyHRB::my_theme()
my_theme <- function(...){
  ggplot2::theme_classic() +
    ggplot2::theme(rect = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(hjust = .5),
          ...)
}
