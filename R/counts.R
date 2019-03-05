# Check if scRNA-Seq data is logged and (or) normalized
#' counts Function
#'
#' Plot the boxplot from ten random columns of the input matrix. Useful to visualize
#' whether the provided count matrix is is logged or if any normalization was done.
#'
#' @export
#' @return the quantiles of the count matrix
counts <- function(count_matrix, n = 10){
  cols <- sample(1:ncol(count_matrix), n)
  boxplot(count_matrix[,cols], pch = 16)
  print(quantile(count_matrix))
}
