#' An auxiliary function to mml_fixed_str().
#'
#' This function calculates probability at each data point for a variable that has no parent. Noticing
#' we only consider no parents variable when it is the target.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param probsMtx probsMtx
#' @param curIndex The current variable's index. The current variable can either be the target or
#' a different variable.
#' @return The function outputs a matrix of probabilities with dimension arity(curIndex) = arity(target)
#' by sample size.
#' @export
probs_adaptive = function(data, arities, sampleSize, probsMtx, curIndex) {

  cnt = rep(1, arities[curIndex]) # initializing cnt with 1
  # initializing probs matrix with 0.5, since the first probs are always 0.5 due to
  # initial count being set to 1
  # each row of probs corresponds to a value of the current (target) var
  # this probs matrix will be used later to obtain the normalizing constant when
  # calculating the condtional probability p(T|Xs)
  for (i in 1:(sampleSize - 1)) {

    k = data[i, curIndex]
    cnt[k] = cnt[k] + 1 # updating cnt
    probsMtx[, i + 1] = cnt / sum(cnt)

  }

  return(probsMtx)

}
