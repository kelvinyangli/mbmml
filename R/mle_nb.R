#' MLE of NB parameters
#'
#' Maximum likelihood of Naive Bayes parameters. Input parameters are always stored in front of output 
#' parameter in the returned list of parameters. 
#' @param data A categorical data set. 
#' @param vars A vector of all variables. 
#' @param xIndices A vector of input variables indices. 
#' @param yIndex The output variable index. 
#' @param smoothing A value for parameter smoothing to avoid having 0 or 1 probabilities. 1 for laplacing 
#' smoothing, 0.5 for mml smoothing. 
#' @export
mle_nb = function(data, vars, xIndices, yIndex, smoothing = 0.5) {
  lst = list()
  if (length(xIndices) > 0) {
    for (i in 1:length(xIndices)) {
      lst[[i]] = t((table(data[, c(yIndex, xIndices[i])]) + smoothing) /
                     (rowSums(table(data[, c(yIndex, xIndices[i])])) + smoothing * nlevels(data[, xIndices[i]])))
    } # end for i 
  }
  # always store y parameters at the last 
  lst[[length(xIndices) + 1]] = 
    (table(data[, yIndex]) + smoothing) / (nrow(data) + smoothing * nlevels(data[, yIndex]))
  names(lst) = c(vars[c(xIndices, yIndex)])
  return(lst)
}