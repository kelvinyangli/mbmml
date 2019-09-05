#' NLL NB for a single data point
#'
#' This is an auxiliary function to calculate the negative log likelihood of the Naive Bayes model for a 
#' single data point given mle of parameters. That is, the function returns partial value of nll, without
#' calculating log(px). The complete value of nll is calculated in mml_nb().
#' @param dataPoint A single data point in the given data set. 
#' @param pars A list of MLE of NB parameters.  
#' @param xIndices A vector of input variables' indices. 
#' @param yIndex The index of the output variable. 
#' @export
nll_nb = function(dataPoint, pars, xIndices, yIndex) {
  ss = 0
  if (length(xIndices) > 0) {
    for (j in 1:(length(pars) - 1)) {
      ss = ss + log(p_ijk(dataPoint, pars, xIndices, xIndices[j], dataPoint[[yIndex]]))
    }
  }
  ss = ss + log(pars[[length(pars)]][dataPoint[[yIndex]]])
  return(ss)
}