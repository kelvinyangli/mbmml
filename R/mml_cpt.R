#' A function to calculate the mml of cpt model
#'
#' There is no difference between mml_cpt_fast() and mml_cpt(), other than the speed difference.
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param arities A vector of varaible arities.
#' @param sampleSize Sample size of the given data set.
#' @param parentsIndices Indices of parents nodes.
#' @param targetIndex Index of the target node.
#' @param alpha A vector of concentration parameters for a Dirichlet distribution. Range is from zeor to positive infinity,
#' length is equal to the arity of the target variable.
#' @param statingPara If TRUE MML estimate of the parameters are also stated with extra 0.5log(pi*e/6) per parameter,
#' otherwise 0.
#' @export
mml_cpt = function(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, alpha, statingPara) {

  if (length(parentsIndices) < 1) {

    msgLen = mml_without_parents(indexListPerNodePerValue, arities, sampleSize, targetIndex, alpha, statingPara)

  } else {

    msgLen = mml_with_parents(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, alpha, statingPara)

  }

  return(msgLen)

}
