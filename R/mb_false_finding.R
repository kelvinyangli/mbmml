#' Evaluating Markov blanket learners according to false findings
#'
#' This function returns the sum of false positives and negatives between the true and learned Markov blanket. 
#' @param mbt The true Markov blanket
#' @param mbl The learned Markov blanket
#' @export
mb_false_finding = function(mbt, mbl) {
  
  fn = length(mbt) - sum(mbt %in% mbl)
  fp = length(mbl) - sum(mbl %in% mbt)
  
  return(fp + fn)
  
}