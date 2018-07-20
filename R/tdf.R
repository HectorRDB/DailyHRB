# Pipe compatible transpose
#' color Function
#'
#' Pipe compatible transpose
#'
#' @param .data the data frame to transpose
#' @param rowname the name of the column that will become colnames, default to NA
#' @param colname the name of the column in the transposed data frame that will contain the old column names. Default to colnames
#' @return a vector with the palette names if print == TRUE
tdf <- function(.data, rowname = NA, colname = "colnames"){
  if(as.character(enquo(rowname)) %in% colnames(.data)){
    return(.data %>% gather(key = colname, value = "values", - !!enquo(rowname)) %>%
             spread(key = !!enquo(rowname), value = "values"))
  } else {
    return(as.data.frame(t(.data), stringsAsFactors = F))
  }
}
