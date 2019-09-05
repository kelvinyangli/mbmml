#' Auxiliary function for mmlLogit
#'
#' This function converts variable levels from strings to numeric starting from 0. For example, if a
#' varaible has levels A, B and C, these three levels will be converted to 0, 1 and 2 respectively. 
#' @param data 
#' @export
factor2numeric = function(data) {
  temp = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  for (i in 1:ncol(data)) {
    # as.numeric convert categrical data values to 1, 2, 3, ...
    # -1 transfers them to 0, 1, 2, ...
    temp[, i] = as.numeric(data[, i]) - 1 
  }
  return(temp)
}
