#' MML Naive Bayes using adaptive code approach (faster version)
#'
#' This function calculates the mml score of a NB model using the adaptive code approach. It is much simpler than using
#' MML87 that involves complex fisher calculation. The output is the mml score of a NB without adding the extra bit
#' for each parameter as what the multi-state mml adaptive code does, because we don't know the complete message length
#' from MML87 for NB. But maybe this is ok, because we are not interested in the mml estimation of parameters.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param arities A vector of variable arities.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param targetIndex The target variable (or parent node) index.
#' @param logProbTarget Log of the probability of the target variable.
#' @param cachedPXGivenT Pre-calculated conditional probability of each node given its parents. In the case of Naive Bayes
#' models, the only parent is the target variable.
#' @param chIndices A vector of indices for the Xs (or child nodes).
#' @export
mml_nb_adaptive = function(data, arities, sampleSize, targetIndex, logProbTarget, cachedPXGivenT, chIndices) {

  lp = logProbTarget
  # a matrix to store the normalizting constant in p(T|Xs)
  margProbs = cachedPXGivenT[[targetIndex]]
  for (x in chIndices) {# go through each node in a given str

    condProbsAdpt = cachedPXGivenT[[x]]
    lp = lp + sum(log(t(condProbsAdpt)[cbind(seq_along(data[, targetIndex]), data[, targetIndex])]))
    # lp = lp + log_prob_adaptive(data, sampleSize, targetIndex, condProbsAdpt)
    margProbs = margProbs * condProbsAdpt

  }

  llh = -(lp - sum(log(apply(margProbs, 2, sum)))) # log(p(T|Xs))

  return(llh)

}







