#' Random permutation of a vector
#'
#' This function randomly permutes a vector or array of length n based on the
#' Algorithm235. It also takes an input that specifies the percentage of hold
#' fixed elements.
#' @param a A random vector or array.
#' @param percentage The percentage of hold fixed elements in the array. It takes
#' real value between 0 and 1, closed interval.
#' @export

random_permutation_with_randomness = function(a, percentage) {

  n = length(a)
  m = round(n * percentage)
  if (m == 0) { # hold fixed none
    a = random_permutation(a)
  } else if (m <= n - 2) { # hold fixed some
    fixedIndices = sample(n, m)
    temp = a[-fixedIndices]
    temp = random_permutation(temp)
    a[-fixedIndices] = temp
  } # else if hold fixed more than n - 2, i.e., only 1 or 0 left, don't permute

  return(a)

}
