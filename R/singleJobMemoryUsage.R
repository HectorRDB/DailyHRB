#' memoryUsage Function
#'
#' Take as input a memory file usage and plot usage over time as well as maximum
#'  usage
#'
#' @param loc the file location
#' @importFrom readr read_table
#' @importFrom stringr str_detect str_remove str_remove_all
#' @importFrom dplyr filter summarise
#' @importFrom magrittr %>%
#' @export

memoryUsage <- function(loc) {
  usage <- read_table(loc, skip = 3) %>%
    filter(X1 == "Mem:")

  usage$used <- lapply(usage$used, function(u) {
    if (str_detect(u, "G")) {
      return(str_remove(u, "G") %>% as.numeric())
    }
    if (str_detect(u, "T")) {
      return((str_remove_all(u, "T") %>% as.numeric()) * 1000)
    }
  }) %>% unlist()

  plot(1:nrow(usage), usage$used, type = "l", ylim = c(0, max(usage$used, na.rm = T)))

  return(
    usage %>% summarise(Max = max(used, na.rm = T),
                        mean = mean(used, na.rm = T))
  )
}
