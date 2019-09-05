#' Random bubble permutation
#'
#' This function randomly permutes an adjacent pair of nodes in a given node order.
#' @param a A random vector or array.
#' @param k an integer parameter indicating how many permutations are allowed.
#' @export

random_bubble_permutation = function(a, k) {

  n = length(a)
  t = 1
  while (t <= k) {
    i = sample(n - 1, 1)
    x = a[i]
    a[i] = a[i + 1]
    a[i + 1] = x
    t = t + 1
  }
  return(a)

}

