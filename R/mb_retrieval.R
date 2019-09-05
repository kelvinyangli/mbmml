#' A function to calculate the classification accuracy of a Markov blanket learner
#'
#' This is a function to calculate the classification accuracy of a Markov blanket learner. In details,
#' the function calculates TP, TN, FP, FN, and returns precision and recall as outputs. If true mb is
#' empty but learned mb is not, then both precision and recall are 0; if learned mb is empty but true
#' mb is not, then both precision and recall are 0; if both true and learned mbs are empty, then both
#' precision and recall are 1.
#' @param mbt The true Markov blanket
#' @param mbl The learned Markov blanket
#' @param nvars The number of variables.
#' @export
mb_retrieval = function(mbt, mbl, nvars) {

  if ((length(mbt) == 0) && (length(mbl) == 0)) {

    precision = recall = 1

  } else if ((length(mbt) == 0) || (length(mbl) == 0)) {

    precision = recall = 0

  } else {

    fn = length(mbt) - sum(mbt %in% mbl)
    fp = length(mbl) - sum(mbl %in% mbt)
    tp = length(mbl) - fp
    tn = nvars - fn - fp - tp
    precision = tp / length(mbl)
    recall = tp / length(mbt)

  }

  return(c(precision, recall))

}






