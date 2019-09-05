#' Auxiliary function to mml_cpt_fast
#'
#' This is an auxiliary function to mml_cpt_fast. It calculates the mml score of a node given its parents.
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param cachedIndicesList A vector of indices stored to speed up calculations.
#' @param arities A vector of varaible arities.
#' @param sampleSize Sample size of a given data set.
#' @param parentsIndices Indices of parents nodes.
#' @param targetIndex Index of the target node.
#' #' @param alpha A vector of concentration parameters for a Dirichlet distribution. Range is from zeor to positive infinity,
#' length is equal to the arity of the target variable.
#' @param statingPara If TRUE MML estimate of the parameters are also stated with extra 0.5log(pi*e/6) per parameter,
#' otherwise 0.
#' @export
mml_with_parents_fast = function(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize,
                                 parentsIndices, targetIndex, alpha, statingPara) {

  newAddedParentIndex = parentsIndices[length(parentsIndices)]
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

  # store curent cachedIndices in a temp list
  tempList = cachedIndicesList
  if (numParents == 1) {# if single parent then just use index i

    for (i in 1:arities[newAddedParentIndex]) {

      cachedIndicesList[[i]] = indexListPerNodePerValue[[parentsIndices]][[i]]
      N_pa_i = length(cachedIndicesList[[i]])
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = single_par_cal(indexListPerNodePerValue, cachedIndicesList[[i]], arityChild, targetIndex, alpha)
      # log(numerator), where numerator = (N(Pa_i) + alpha0 - 1)!
      # log_gamma(n+1) is an approximation of log(factorial(n))
      logNumerator = log_gamma((N_pa_i + alpha0))
      nonFixedTerm = nonFixedTerm + logNumerator + constant1 - constant2 - cumSum

    }

  } else {# if multiple parents

    j = 1
    for (i in 1:length(cachedIndicesList)) {

      for (ii in 1:arities[newAddedParentIndex]) {

        cachedIndicesList[[j]] = intersect(tempList[[i]], indexListPerNodePerValue[[newAddedParentIndex]][[ii]])
        N_pa_i = length(cachedIndicesList[[j]])
        cumSum = multi_pars_cal(indexListPerNodePerValue, cachedIndicesList[[j]], arityChild, targetIndex, alpha)
        # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
        logNumerator = log_gamma(N_pa_i + alpha0)
        nonFixedTerm = nonFixedTerm + logNumerator + constant1 - constant2 - cumSum
        j = j + 1

      } # end for ii

    } # end for i

  } # end else

  msgLen = constantDiff + nonFixedTerm
  ls = list(msgLen = msgLen, cachedIndicesList = cachedIndicesList)

  return(ls)

}
