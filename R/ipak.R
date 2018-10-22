# Load vector of packages, install if needed
#' ipak Function
#'
#' This function loads the packages and install them from CRAN or Bioconductor if needed
#'
#' @param pcks a vector of characters naming packages
#' @export
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    BiocManager::install(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
