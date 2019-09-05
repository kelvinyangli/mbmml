#' Random permutation of a vector
#'
#' This function randomly permutes a vector or array of length n based on the
#' Algorithm235.
#' @param a A random vector or array.
#' @export

random_permutation = function(a) {

  n = length(a)
  for (i in n:2) {
    j = sample(i, 1)
    b = a[i]
    a[i] = a[j]
    a[j] = b
  }
  return(a)

}
