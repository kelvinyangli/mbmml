#' A function to calculate FIM entries
#'
#' This function calculates the finsher information matrix of a 1st order logit model. 
#' @param dataNumeric Numerical format of original data, where variable values start from 0.  
#' @param betaDotX Inner product of parameters beta and input variables X. 
#' @param xIndices A vector of indices of the input variables.
#' @param y The index of the output/target variable. 
#' @details FIM is a square matrix with dimensions equal to |beta| = |xIndices| + 1 for binary logit models.
#' rowID and colID in FIM corresponds to rowID-1 and colID-1 in xIndices, because the first row and column 
#' of FIM is the 2nd derivative w.r.t. beta_0. There is no need to add a column of 1s in front of 
#' dataNumeric, because the function start filling in entries from 2nd row and 2nd column. The 1st row and 
#' column are filled in separately. 
#' @export
fim_logit = function(dataNumeric, betaDotX, xIndices, yIndex) {
  m = matrix(0, length(xIndices) + 1, length(xIndices) + 1) 
  # fill in the (1, 1) entry 
  m[1, 1] = sum(exp(betaDotX) / (1 + exp(betaDotX)) ^ 2)
  for (rowID in 2:nrow(m)) {# start from 2nd row
    for (colID in 2:rowID) {# start from 2nd col, because 1st col is identical to diagonal 
      m[rowID, colID] = (exp(betaDotX) / (1 + exp(betaDotX)) ^ 2) %*% (dataNumeric[, xIndices[rowID - 1]] * dataNumeric[, xIndices[colID - 1]])
    } 
  }  
  m[, 1] = diag(m)
  m[upper.tri(m)] = t(m)[upper.tri(t(m))] # duplicate entries for upper half of fim
  return(m) 
}