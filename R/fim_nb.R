#' Calculate FIM of NB
#'
#' This function calculates the expected fisher information matrix of a Naive Bayes model. Analytical 
#' solutions of the hessian of the nll have been numerically verified using the hessian() function
#' in library(numDeriv). The expected hessian are not too far from the observed. In
#' fact, only diagonal entries of the hessian contain y, so expectations need to be calculated. But the 
#' learned fim can still have negative determinant (very small negative determinant, such as -1e-20). Maybe 
#' this is due to underflow? FIM of NB doesn't seem to be positive definite like FIM of logit, so can't 
#' use cholesky decomposition. 
#' @param probSign A data frame with 1 and -1, which corresponds to the 1st and 2nd level of a varaible. 
#' It is used for computing the FIM of Naive Bayes.
#' @param prodPij1 A vector of \prod_j p(x_ij|y=1).
#' @param prodPij0 A vector of \prod_j p(x_ij|y=0).
#' @param px A vector of py1 * prodPij1 + py0 * prodPij0.
#' @param probsMatrix A matrix of p(x_j|y=1) and p(x_j|y=0)
#' @param py1 p(y=T)
#' @param py0 p(y=F)
#' @param arities A vector of variable arities.
#' @param xIndices A vector of input variables indices.
#' @param yIndex The index of the output variable. 
#' @export
fim_nb = function(probSign, prodPij1, prodPij0, px, probsMatrix, py1, py0, arities, xIndices, yIndex) {
  # py1 = <qi1>
  # py0 = <1 - qi1>
  # prodPij1 = <\pi_{i1}>
  # prodPij0 = <\pi_{i0}>
  
  if (length(xIndices) == 1) {
    summation = sum
  } else if (length(xIndices) > 1) {
    summation = colSums
  }
  
  # empty FIM
  fimDim = (arities[yIndex] - 1) + length(xIndices) * arities[yIndex]
  fim = matrix(0, nrow = fimDim, ncol = fimDim) 
  
  # off diagonal entries
  cc = prodPij1 * prodPij0 / (px ^ 2) # a common contant 
  # 1. dl^2/qi0*qik1
  fim[1, 2:(length(xIndices) + 1)] = summation(cc / (probsMatrix[, 1:length(xIndices)] * probSign[, xIndices]))
  # 2. dl^2/qi0*qik0
  fim[1, (length(xIndices) + 2):ncol(fim)] = 
    -summation(cc / (probsMatrix[, (length(xIndices) + 1):ncol(probsMatrix)] * probSign[, xIndices]))
  
  # only fill-in these entries when there are more than 1 x
  if (length(xIndices) > 1) {
    # 3. dl^2/dpik1*dpil1, where k \in [1, m - 1] and l \in [k + 1, m]
    # 4. dl^2/dpik0*dpil0, where k \in [1, m - 1] and l \in [k + 1, m]
    for (k in 1:(length(xIndices) - 1)) {
      for (l in (k + 1):length(xIndices)) {
        fim[k + 1, l + 1] = 
          sum(py1 * py0 * cc / apply(probsMatrix[, c(k, l)] * probSign[, xIndices[c(k, l)]], 1, prod))
        fim[k + length(xIndices) + 1, l + length(xIndices) + 1] = 
          sum(py1 * py0 * cc / apply(probsMatrix[, c(k, l) + length(xIndices)] * probSign[, xIndices[c(k, l)]], 1, prod))
      }
    }
  }
  
  # 5. dl^2/dpik1*dpil0, where k, l \in [1, m]
  for (k in 1:length(xIndices)) {
    for (l in 1:length(xIndices)) {
      fim[k + 1, l + length(xIndices) + 1] = 
        -sum(py1 * py0 * cc / apply(probsMatrix[, c(k, l + length(xIndices))] * probSign[, xIndices[c(k, l)]], 1, prod))
    }
  }
  
  # duplicate upper to lower triangular fim
  fim = fim + t(fim) 
  
  # diagonal entries
  # assume all variables are binary, hence the diag[1] is always the 2nd derivative w.r.t. q_i0
  diag(fim)[1] = sum(prodPij1 / (py1 * px) + prodPij0 / (py0 * px) - ((prodPij1 - prodPij0) / px) ^ 2)
  # rest of the diagonals
  diag(fim)[-1] = colSums(py1 * py0 * prodPij1 * prodPij0 / (px * probsMatrix) ^ 2)
  
  return(fim)
  
}
