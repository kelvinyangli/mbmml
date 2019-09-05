#' Calculate the negative log likelihood of an entire data set
#'
#' This function calculate the negative log likelihood of an entire data set.
#' @param dataNumeric The numeric format of the given data set. Variable values start from 0. Obtained by 
#' using the function factor2numeric(). 
#' @param betaDotX Inner product of logit parameters beta and a data point X. 
#' @param xIndices Indices of the input variables.
#' @param yIndex Input of the output/target variable. 
#' @export
nll_logit = function(dataNumeric, betaDotX, xIndices, yIndex) {
  if (length(xIndices) < 1) {
    ll = -log(1 + exp(betaDotX)) * nrow(dataNumeric) + sum(dataNumeric[, yIndex]) * betaDotX
  } else {
    ll = sum(-log(1 + exp(betaDotX))) + as.vector(dataNumeric[, yIndex] %*% betaDotX)
  }
  return(-ll) 
}