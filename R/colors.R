# Print colors
#' color Function
#'
#' Print the palettes of RColorBrewer
#'
#' @param print If TRUE, print the palette names
#' @return a vector with the palette names if print == TRUE
#' @importFrom RColorBrewer display.brewer.all brewer.pal.info
#' @export
#' @examples
#' DailyHRB::colors()
colors <- function(print = FALSE){
  RColorBrewer::display.brewer.all()
  if (print) {
    rownames(RColorBrewer::brewer.pal.info)
  }
}
