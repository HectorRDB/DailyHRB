#Better graphics
.onAttach <- function(libname, pkgname) {
  library(tidyverse)
  theme_set(theme_classic())
}
