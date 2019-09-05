#' Approximation of the mutual information between two variables
#'
#' This function approximates the mutual information between two variables based
#' on the given data.
#' @param data A categorical dataset.
#' @param n Sample size.
#' @param x A random variable X.
#' @param y A random variable Y.
#' @param logBase The base of logarithm. Default is 2.
#' @param smth A small value to get rid of 0 occurence.
#' @export
aprx_mi = function(data, n, x, y, logBase = 2, smth = 1e-10) {
  pxy = as.vector(table(data[, c(x, y)]) / n)
  px = as.vector(table(data[, x]) / n)
  py = as.vector(table(data[, y]) / n)
  pxpy = rep(0, length(pxy))
  cnt = 1
  for (j in 1:length(py)) {
    for (i in 1:length(px)) {
      pxpy[cnt] = px[i] * py[j]
      cnt = cnt + 1
    }
  }
  # add smoothing, in case 0 prob
  pxy = pxy + e
  pxpy = pxpy + e
  return(sum(pxy * (log(pxy, logBase) - log(pxpy, logBase))))

}



