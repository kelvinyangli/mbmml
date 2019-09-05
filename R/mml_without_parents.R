#' Auxiliary function to both mml_cpt and mml_cpt_fast
#'
#' This is an auxiliary function to both mml_cpt() and mml_cpt_fast(). It calculates the mml score of
#' a node without any parents.
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param arities A vector of variable ariteis.
#' @param sampleSize Sample size of a given data set.
#' @param targetIndex Index of the target node.
#' @param alpha A vector of concentration parameters for a Dirichlet distribution. Range is from zeor to positive infinity,
#' length is equal to the arity of the target variable.
#' @param statingPara If TRUE MML estimate of the parameters are also stated with extra 0.5log(pi*e/6) per parameter,
#' otherwise 0.
#' @export
mml_without_parents = function(indexListPerNodePerValue, arities, sampleSize, targetIndex, alpha, statingPara = FALSE) {

  arity = arities[targetIndex]
  alpha0 = sum(alpha)
  constantDiff = 0
  if (statingPara) constantDiff = 0.5 * (arity - 1) * log((pi * exp(1) / 6))
  # log_gamma(n+1) is an approximation of log(factorial(n))
  fixedTerm = constantDiff + log_gamma(sampleSize + alpha0) + sum(sapply(alpha, log_gamma)) - log_gamma(alpha0)
  cumSum = 0
  for (i in 1:arity) {

    N_x_i = length(indexListPerNodePerValue[[targetIndex]][[i]])
    cumSum = cumSum + log_gamma(N_x_i + alpha[i])

  }

  return(fixedTerm - cumSum)

}
