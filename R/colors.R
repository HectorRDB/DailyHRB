# Print colors
#' color Function
#'
#' Print the palettes of RColorBrewer
#'
#' @param print If TRUE, print the palette names
#' @return a vector with the palette names if print == TRUE
#' @export
colors <- function(print = F){
  library(RColorBrewer)
  RColorBrewer::display.brewer.all()
  if (print) {
    rownames(RColorBrewer::brewer.pal.info)
  }
}
