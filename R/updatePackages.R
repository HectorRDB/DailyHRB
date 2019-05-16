# After updating R, reinstall all packages that where installed on the machine
#' updatePackages Function
#'
#' This function needs only to be run when R is updated (i.e once a year). It will look at the old list of packages that where installed and will try to install them. It will also returns a list of the packages that failed (probabvly mostly Github packages).
#'
#' @param oldLib where the old packages where stored
#' @param newLib where the new packages will be stored
#' @export
#' @import BiocManager
updatePackages <- function(oldLib, newLib = .libPaths()[1]) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  install(update = TRUE, ask = FALSE)
  packages <- as.data.frame(installed.packages(oldLib, stringsAsFactors = F))
  for (package in packages$Package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      try(install(package, lib = newLib, update = FALSE, ask = FALSE))
    }
  }
  packages2 <- as.data.frame(installed.packages(newLib, stringsAsFactors = F))
  failed <- dplyr::anti_join(packages, packages2, by = c("Package" = "Package"))
  return(failed)
}


