#' A faster and accurate approximation to log factorial of a non-negative integer/real number
#'
#' This function is more accuarte than Stirling's approximation. For more details, please check on
#' https://en.wikipedia.org/wiki/Lanczos_approximation. The log is natural base.
#' @param z A positive integer or real number.
#' @export
log_gamma = function(z) {
  # Coefficients used by the GNU Scientific Library
  p = c(676.5203681218851, -1259.1392167224028, 771.32342877765313,
        -176.61502916214059, 12.507343278686905, -0.13857109526572012,
        9.9843695780195716e-6, 1.5056327351493116e-7)
  if (z < 0.5) {
    y = log(pi) - log(sin(pi * z)) - log_gamma(1 - z)
  } else {
    z = z - 1
    x = 0.99999999999980993
    for (i in 1:length(p)) {
      x = x + p[i] / (z + i)
    }
    t = z + 7 + 0.5
    y = 0.5 * log(2 * pi) + (z + 0.5) * log(t) - t + log(x)
  }
  return(y)
}
