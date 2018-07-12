# Print colors
colors <- function(print = F){
  library(RColorBrewer)
  RColorBrewer::display.brewer.all()
  if(print){
    rownames(RColorBrewer::brewer.pal.info)
  }
}
