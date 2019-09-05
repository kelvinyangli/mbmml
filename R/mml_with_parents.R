#' Auxiliary function to mml_cpt()
#'
#' This function calculates the mml score of a target node given its parents. There has to be at least one
#' parent for the target. Natural log is used.
#' @param indexListPerNodePerValue indexListPerNodePerValue
#' @param arities arities
#' @param sampleSize sampleSize
#' @param parentsIndices parentsIndices
#' @param targetIndex targetIndex
#' @param alpha A vector of concentration parameters for a Dirichlet distribution. Range is from zeor to positive infinity,
#' length is equal to the arity of the target variable.
#' @param statingPara If TRUE MML estimate of the parameters are also stated with extra 0.5log(pi*e/6) per parameter,
#' otherwise 0.
#' @export
mml_with_parents = function(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, alpha,
                            statingPara = FALSE) {

  arityChild = arities[targetIndex]
  alpha0 = sum(alpha)
  numParents = length(parentsIndices)
  numParentsInstantiations = prod(arities[parentsIndices])
  # sum(log(alpha_j - 1)!)
  constant1 = sum(sapply(alpha, log_gamma))
  # log((alpha0 - 1)!)
  constant2 = log_gamma(alpha0)
  nonFixedTerm = 0
  constantDiff = 0
  if (statingPara) constantDiff = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6))

  for (i in 1:numParentsInstantiations) {

    if (numParents == 1) { # if single parent then just use index i

      commonParentsIndices = indexListPerNodePerValue[[parentsIndices]][[i]]

      N_pa_i = length(commonParentsIndices)

      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = single_par_cal(indexListPerNodePerValue, commonParentsIndices, arityChild, targetIndex, alpha)

    } else { # if more than 1 parent, use function to get potential combination

      # fix this part
      potentialCombination = get_parents_instantiation_indices(arities, numParents, parentsIndices,
                                                               numParentsInstantiations, i)

      commonParentsIndices = intersect_indices(numParents, parentsIndices, indexListPerNodePerValue, potentialCombination)
      #ll[[i]]=commonParentsIndices
      N_pa_i = length(commonParentsIndices)

      cumSum = multi_pars_cal(indexListPerNodePerValue, commonParentsIndices, arityChild, targetIndex, alpha)

    } # end if else

    # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
    # log_gamma(n+1) is an approximation of log(factorial(n))
    logNumerator = log_gamma(N_pa_i + alpha0)
    #cat(logNumerator - logConstant - cumSum, "\n")
    nonFixedTerm = nonFixedTerm + logNumerator + constant1 - constant2 - cumSum

    #cat(logNumerator - logConstant - cumSum, "\n")
  } # end for i

  return(constantDiff + nonFixedTerm)

}
