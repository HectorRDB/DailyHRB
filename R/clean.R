# Load vector of packages, install if needed
#' clean Function
#'
#' This function remove columns of a dataframe that provide no info (identitcal accross all rows)
#'
#' @param tb the \code{data.frame} or \code{tibble}
#' @export
clean <- function(tb){
  cols <- colnames(tb)[
    map_int(tb, function(x) length(unique(x))) > 1]
  tb %>% select(!!cols)
}
