#' A function to calculate log of the determinant of a matrix
#' 
#' This function calculates the log (natural base) of the determinant of a matrix in order to avoid overflow
#' for some large determinant. The matrix has to be symmetric positive-definite in order to use the 
#' cholesky decomposition. 
#' @param x A symmetric positive-definite square matrix. 
#' @export
log_determinant = function(x) {
  choleskeyUpper = chol(x) # choleski decomposition
  logDet = sum(log(diag(choleskeyUpper)))
  return(logDet * 2)
}