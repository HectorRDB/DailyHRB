# Load vector of packages, install if needed
#' clean Function
#'
#' This function remove columns of a dataframe that provide no info (identitcal accross all rows) or
#' disinct accross all rows. Useful to clean metadata files.
#'
#' @param tb the \code{data.frame} or \code{tibble}
#' @param unique whether to keep unique columns
#' @param keep a vector of columns to keep
#' @importFrom purrr map_int
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
#' @examples
#' metaData <- data.frame(SRA = "SRA17CJQ1",
#'                        ID1 = sample(letters, 12, replace = FALSE),
#'                        group = c(rep("group1", 4),
#'                                  rep("group2", 4),
#'                                  rep("group3", 4)))
#' metaData$ID2 <- toupper(metaData$ID1)
#' metaData
#' clean(metaData)
#' clean(metaData, unique = FALSE, keep = "ID1")
clean <- function(tb, unique = TRUE, keep = NULL){
  if (unique) {
    cols <- colnames(tb)[map_int(tb, function(x) length(unique(x))) > 1]
  } else {
    cols <- colnames(tb)[
      map_int(tb, function(x) length(unique(x))) > 1 &
        map_int(tb, function(x) length(unique(x)))  < ncol(tb)]
  }
  cols <- c(cols, keep) %>% unique()
  tb %>% select(!!cols)
}
