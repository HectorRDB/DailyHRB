# Load vector of packages, install if needed
#' ipak Function
#'
#' This function loads the packages and install them from CRAN or Bioconductor if needed
#'
#' @param pkg a vector of characters naming packages
#' @export
#' @examples
#' DailyHRB::ipak(c("ggplot2", "Biobase"))
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    if (!"BiocManager" %in% installed.packages()) {
      install.pacakges("BiocManager")
    }
    BiocManager::install(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
