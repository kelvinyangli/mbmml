#' Calculate inner product of logit parameters and input variables
#'
#' This function calculates the inner product of the logit parameters and the input variables given a single
#' data point. 
#' @param dataPoint A single data point. That is, a single row of an entire data set. 
#' @param beta A vector of logit model parameters. 
#' @param xIndices Indices of input variables.
#' @export
inner_product = function(dataPoint, beta, xIndices) {
  if (length(xIndices) < 1) {# if x is empty
    res = beta
  } else {
    res = beta[1] + beta[-1] %*% dataPoint[xIndices]
    # the above inner product operator %*% returns value in a matrix format
    # convert it to a vector of single value 
    res = as.vector(res) 
  }
  return(res)
}