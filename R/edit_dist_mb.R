#' A function to calculate the edit distance from the learned Markov blanket to the true Markov blanket
#'
#' This is a function to calculate the edit distance from the learned Markov blanket to the true 
#' Markov blanket. Edit distance is defined as the number of operations required to get from one to the 
#' other. If both true and learned are empty, the edit distance is 0; if only one of them is 
#' empty, the edit distance is the size of the other; if none of them is empty, the edit distance is 
#' the false positives plus the false negatives. 
#' @param mbTrue The true Markov blanket
#' @param mbLearned The learned Markov blanket
#' @param vars A vector of all variables. 
#' @param target The target node. 
#' @export
edit_dist_mb = function(mbTrue, mbLearned, vars, target) {
  if ((length(mbLearned) == 0) && (length(mbTrue) == 0)) { # if both true and learned mbs are empty
    d = 0
  } else if ((length(mbLearned) > 0) && (length(mbTrue) == 0)) {
    d = length(mbLearned)
  } else  if ((length(mbLearned) == 0) && (length(mbTrue) > 0)) {
    d = length(mbTrue)
  } else {
    nonMBTrue = vars[!vars %in% mbTrue] # remove true mb from all nodes
    nonMBTrue = nonMBTrue[nonMBTrue != target] # then remove target node 
    nonMBLearned = vars[!vars %in% mbLearned] # remove learned mb from all nodes
    nonMBLearned = nonMBLearned[nonMBLearned != target] # then remove target node 
    TP = sum(mbLearned %in% mbTrue) # the number of correct findings
    FP = length(mbLearned) - TP
    TN = sum(nonMBLearned %in% nonMBTrue) # the number of correct excluding nodes
    FN = length(nonMBLearned) - TN
    d = FP + FN
  }
  return(d)
}


