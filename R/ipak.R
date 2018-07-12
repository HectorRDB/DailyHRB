# Load vector of packages, install if needed

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    biocLite(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
