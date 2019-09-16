# Check if scRNA-Seq data is logged and (or) normalized
#' checkType Function
#'
#' Plot the boxplot from ten random columns of the input matrix. Useful to visualize
#' whether the provided count matrix is is logged or if any normalization was done.
#'
#' @export
#' @return the quantiles of the count matrix
#' @param count_matrix The matrix that we want to check
#' @param n How many columns to check. Default to 10.
#' @importFrom stats quantile
#' @importFrom graphics boxplot plot
#' @examples
#' data <- matrix(rnbinom(1000 * 100, size = 1000, prob = .999), ncol = 100)
#' checkType(data)
checkType <- function(count_matrix, n = 10){
  cols <- sample(1:ncol(count_matrix), n)
  boxplot(count_matrix[,cols], pch = 16)
  print(quantile(count_matrix))
}
