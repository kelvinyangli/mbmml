#' Approximation of the KL-Divergence between two distributions.
#'
#' This function approximates from the learned distribution to the true distribution.
#' This is not the full kld, it is the last term in Acid and de Campos (2003)'s paper
#' on page 37. This function only approximates the kld, because the distributions
#' are estimated by MLE. The reason to not use the true distribution is to get
#' fast calculation by approximating the joint distributions from data.
#' @param data A categorical dataset.
#' @param cptsLearned The estimated distribution of a learned BN.
#' @param logBase The base of logarithm. Default is 2.
#' @param smth A small value to get rid of 0 occurence.
#' @export
aprx_kld = function(data, cptsLearned, logBase = 2, smth = 1e-10) {
  n = nrow(data)
  vars = colnames(data)
  mi = rep(0, length(vars))
  for (i in 1:length(vars)) {
    x = vars[i]
    y = cptsLearned[[x]]$parents
    # only deal with nodes who has at least one parent
    if (length(y) > 1) {
      mi[i] = aprx_mi(data, n, x, y, logBase, smth)
    }
  }
  return(sum(mi))
}
