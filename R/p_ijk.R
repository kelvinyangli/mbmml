#' Auxiliary function to fim_nb()
#' 
#' This is an auxiliary function to fim_nb(). It calculates p(x_ij|y=k) for a single data point and store it
#' in a vector for future use. It is mainly for speed up the calculation of fim_nb(). 
#' @param dataPoint A single data point in the given data set. 
#' @param pars A list of MLE of NB parameters.  
#' @param xIndices A vector of input variables' indices. 
#' @param xIndex The idnex of a particular input variable in colnames(data).
#' @param yValue The value of the output variable. The current version of mml_nb only takes binar variables, 
#' so the possible y values are {0, 1}.
#' @export
p_ijk = function(dataPoint, pars, xIndices, xIndex, yValue) {
  xValue = dataPoint[[xIndex]]
  xParsIndex = which(xIndices == xIndex)
  return(pars[[xParsIndex]][xValue, yValue])
}