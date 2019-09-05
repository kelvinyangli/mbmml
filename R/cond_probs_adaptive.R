#' An auxiliary function to mml_fixed_str()
#'
#' This function calculates conditional probability at each data point for a variable if it is the
#' target or it has at least one parent.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in.
#' @param probsMtx probsMtx
#' @param curIndex The current variable's index. The current variable can either be the target or
#' a different variable.
#' @param curPaIndices The indices of the current variable's parents.
#' @return The function outputs a matrix of conditional probabilities with dimension arity(target) by
#' sample size.
#' @export
cond_probs_adaptive = function(data, arities, sampleSize, targetIndex, probsMtx, curIndex, curPaIndices) {

  ind = which(c(curPaIndices, curIndex) == targetIndex)
  cnt = rep(1, arities[curIndex]) # initializing cnt with 1
  for (i in rev(curPaIndices)) {

    # make cnt a high-dim list to store variable count
    # each lvl in the list corresponds to one value of one pa
    # e.g. if two binary parents, then cnt is a list with 4 lvls
    # the rev(curPaIndices) ensures the 1st pa's 1st value is in the
    # most inner lvl
    cnt = rep(list(cnt), arities[i])

  }

  # initializing prob matrix with 0.5, since the first probs are always 0.5 due to
  # initial count being set to 1
  # each row of probs corresponds to a value of the target var
  # this probs matrix will be used later to obtain the normalizing constant when
  # calculating the condtional probability p(T|Xs)
  for (i in 1:(sampleSize - 1)) {# counting adaptively at each data point

    indices = data[i, c(curPaIndices, curIndex)]
    cnt[[matrix(indices, 1)]] = cnt[[matrix(indices, 1)]] + 1 # updating cnt
    indices = data[i + 1, c(curPaIndices, curIndex)] # computing prob from 2nd data point
    for (k in 1:arities[targetIndex]) {# loop through each value of the target

      indices[ind] = k
      probsMtx[k, i + 1] = cnt[[matrix(indices, 1)]] / sum(cnt[[matrix(indices[-length(indices)], 1)]])

    }

  } # end adaptive counting

  return(probsMtx)

}
