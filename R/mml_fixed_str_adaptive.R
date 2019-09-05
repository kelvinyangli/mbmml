#' MML for a fixed Markov blanket model using adaptive code approach
#'
#' This function calculates the mml score of the target under a fixed Markov blanket model. The structure
#' can be a general DAG. This returns the message length without stating the mml estimate of the parameters.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in.
#' @param logProbTarget Log of the probability of the target.
#' @param cachedPXGivenT cachedPXGivenT
#' @param probsMtx probsMtx
#' @param str A fixed Markov blanket model stores in the bnlearn dag format.
#' @param mbIndices The indices of potential Markov blanket variables.
#' @param cachedPXGivenY cachedPXGivenY
#' @param cachInd cachInd
#' @return The function outputs the message length of a fixed structure.
#' @keywords This function has dependencies on cond_probs_adaptive().
#' @export
mml_fixed_str_adaptive = function(data, vars, arities, sampleSize, targetIndex, logProbTarget,
                                  cachedPXGivenT, probsMtx, str, mbIndices, cachedPXGivenY, cachInd) {

  lp = 0
  # a matrix to store the normalizting constant in p(T|Xs)
  margProbs = matrix(1, arities[targetIndex], sampleSize)

  for (curIndex in c(targetIndex, mbIndices)) {# go through each node in a given str

    # if it has at least one parent,
    # then get the adaptive count of it given its parent set
    curPa = which(str[, vars[curIndex]] == 1)

    if (length(curPa) > 0) {

      curPaIndices = which(vars %in% names(curPa))
      ind = which(names(cachedPXGivenY) == paste(c(curIndex, curPaIndices), collapse = ""))
      if (length(ind) > 0) {

        condProbsAdpt = cachedPXGivenY[[ind]]

      } else {# cach condProbsAdpt if it hasn't been cached

        condProbsAdpt = cond_probs_adaptive(data, arities, sampleSize, targetIndex, probsMtx, curIndex, curPaIndices)
        cachedPXGivenY[[cachInd]] = condProbsAdpt
        # assign an unique name as primary key to look up existing cached PTs
        names(cachedPXGivenY)[cachInd] = paste(c(curIndex, curPaIndices), collapse = "")
        cachInd = cachInd + 1

      }

      lpEachNode = sum(log(t(condProbsAdpt)[cbind(seq_along(data[, targetIndex]), data[, targetIndex])]))
      margProbs = margProbs * condProbsAdpt
      lp = lp + lpEachNode

    } else if ((curIndex == targetIndex) && (length(curPa) < 1)) {

      lpEachNode = logProbTarget
      margProbs = margProbs * cachedPXGivenT[[targetIndex]]
      lp = lp + lpEachNode

    } # end if else

  }

  llh = -(lp - sum(log(apply(margProbs, 2, sum)))) # log(p(T|Xs))
  lst = list(llh = llh, cachedPXGivenY = cachedPXGivenY, cachInd = cachInd)
  return(lst)

}

