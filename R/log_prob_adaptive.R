#' An auxiliary function to mml_fixed_str()
#'
#' This function calculates log of the conditional probability for a variable given its parents. It is used
#' to obtain p(x|pa of x) in the numerator of the conditional prob p(T|Xs).
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in.
#' @param condProbsAdpt A conditional probabilities matrix that stores a variable's conditional probability
#' at each data point. This variable is either the target, or has more than one parent.
#' @return The function outputs a single log probability value.
#' @export
log_prob_adaptive = function(data, targetIndex, condProbsAdpt) {

  # i = data[, targetIndex]
  lp = sum(log(t(condProbsAdpt)[cbind(seq_along(data[, targetIndex]), data[, targetIndex])]))

  # lp = 0
  # for (i in 1:sampleSize) {
  #
  #   targetValue = data[i, targetIndex]
  #   lp = lp + log(condProbsAdpt[targetValue, i])
  #
  # }

  return(lp)

}

