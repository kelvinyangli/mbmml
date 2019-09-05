#' Kendall tau distance
#'
#' Kendall tau distance is the number of pairs of nodes that are in a reversed
#' order in an array a and an array b.
#' @param a A random vector or array.
#' @param b A random vector or array.
#' real value between 0 and 1, closed interval.
#' @export

kendall_tau_distance = function(a, b) {

  d = 0
  n = length(a)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      k = which(b == a[i])
      l = which(b == a[j])
      if (l < k) d = d + 1
    }
  }
  return(d)

}
