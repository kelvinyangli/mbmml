#' Auxiliary function to fim_nb()
#' 
#' This is an auxiliary function to fim_nb(). It calculates \prod_j p(x_ij|y=k) for a single data point 
#' and store it in a vector for future use. It is mainly for speed up the calculation of fim_nb(). Notice 
#' that this function is almost the same as p_ijk(), except that this function calculte the product over
#' all input variables x_j. 
#' @param dataPoint A single data point in the given data set. 
#' @param pars A list of MLE of NB parameters.  
#' @param xIndices A vector of input variables' indices. 
#' @param yValue The value of the output variable. The current version of mml_nb only takes binar variables, 
#' so the possible y values are {0, 1}.
#' @export
prod_pijk = function(dataPoint, pars, xIndices, yValue) {
  mm = 1
  for (i in 1:length(xIndices)) {
    xValue = dataPoint[[xIndices[i]]]
    mm = mm * pars[[i]][xValue, yValue]
  }
  return(mm)
}