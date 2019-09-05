#' A faster version of a forward greedy search
#'
#' This is a faster version of a forward greedy search. It can only be used with mml_cpt_fast()
#' as the objective function.
#' @param data A categorical data set.
#' @param varCnt A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param arities A vector of arities.
#' @param vars vars
#' @param sampleSize Sample size of the given data.
#' @param target The target node.
#' @param alpha A vector of concentration parameters for a Dirichlet distribution. Range is from zeor to positive infinity,
#' length is equal to the arity of the target variable.
#' @param statingPara Default is FALSE. If TRUE, then MML estimate of the parameters are also stated with extra 0.5log(pi*e/6)
#' per parameter.
#' @param debug A boolearn argument to display more details.
#' @export
forward_greedy_fast = function(data, varCnt, arities, vars, sampleSize, target, alpha = 1, statingPara = FALSE, debug = FALSE) {

  targetIndex = which(vars == target) # get index of the target node
  nvars = ncol(data)
  mb = c()
  # initialMML = rep(0, nvars)
  unCheckedIndices = (1:nvars)[-targetIndex]
  tempCachedIndicesList = list()

  if (prod(alpha == 1)) alpha = rep(1, arities[targetIndex])
  minMsgLen = mml_cpt(varCnt, arities, sampleSize, c(), targetIndex, alpha, statingPara)
  # localOptimalMML = minMsgLen
  if (debug) {
    cat("Search: Forward greedy with mmlCPT \n")
    cat("0 parent:", minMsgLen, "\n")
  }

  # repeat the process of computing mml for remaining unCheckedIndices
  # if unCheckedIndices is empty or all msg len > min msg len then stop
  repeat {

    index = 0 # initialize index to 0
    cachedIndicesList = tempCachedIndicesList
    if (length(unCheckedIndices) == 0) {
      if (debug) cat("BM is full! \n")
      break
    }

    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      parentsIndices = c(mb, unCheckedIndices[i])
      res = mml_cpt_fast(varCnt, cachedIndicesList, arities, sampleSize, parentsIndices, targetIndex, alpha, statingPara)
      msgLenCurrent = res$msgLen
      #cachedIndicesList = res$cachedIndicesList

      if (debug) cat("parents =", vars[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")

      # if the current msg len is smaller then replace minMsgLen by the current
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) {
        minMsgLen = msgLenCurrent
        index = i
        tempCachedIndicesList = res$cachedIndicesList
      } # end if

      # record all initial mml scores and the vars
      # if (length(mb) < 1) initialMML[unCheckedIndices[i]] = msgLenCurrent
      #names(initialMML)[i] = vars[unCheckedIndices[i]]
    } # end for i

    if (index == 0) {
      if (debug) cat("Stop! No better choice for MB! \n")
      break
    } else {
      if (debug) cat("add", vars[unCheckedIndices[index]], "into mb \n")
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      if (debug) cat("current mb is {", vars[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      unCheckedIndices = unCheckedIndices[-index]

      # keep track of local optimal mml scores
      # localOptimalMML = c(localOptimalMML, minMsgLen)
    } # end else
  } # end repeat

  # lst = list("mb" = vars[mb], "initial" = initialMML[mb], "optimal" = localOptimalMML)
  return(vars[mb])
  # return(lst)

}



