#' Auxiliary to fim_nb()
#'
#' This function makes a matrix of 1 and -1 that is used to change sign of pik0 and pik1 when calculating 
#' FIM. When x[i, k] = 1 (i.e., 1st level), then 1, meaning no change; when x[i, k] = 2 (i.e., 2nd level), 
#' then -1, meaning change sign, since this parameter is expressed by 1 - p(xi = 1|y=1 or 0).
#' @param data A binary data set. 
#' @param xIndices A vector of input variables indices.
#' @export
get_prob_sign = function(data) {
  mtx = matrix(1, nrow = nrow(data), ncol = ncol(data))
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (as.numeric(data[i, j]) == 2) mtx[i, j] = -mtx[i, j]
    }
  }
  mtx = as.data.frame(mtx)
  colnames(mtx) = colnames(data)
  return(mtx)
}