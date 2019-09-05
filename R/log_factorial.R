#' A faster function to calculate log factorial of a non-negative integer
#'
#' This is a relatively faster function to calculate the log factorial of a non-negative integer n. If n = 1, 
#' the result is 0; if n <= 10000, the result is read from a pre-saved log factorial sheet; if n > 10000, use
#' stirling's approximation. 
#' @param logFactorialSheet A pre-saved csv file that contains log factorial for n <= 10000. 
#' @param n A non-negative integer.  
#' @param base The base of a logarithm. 
#' @export
# log_factorial = function(logFactorialSheet, n, base) {
#   if (n < 2) {
#     lf = 0
#   } else if (n <= 10000) { # read from logFactorialSheet
#     lf = logFactorialSheet[n, ceiling(exp(1)/base)]
#   } else { # use stirling's approximation
#     lf =   n * log(n, base) - n * log(exp(1), base) + 0.5 * (log(n, base) + log(2 * pi, base))
#   }
#   return(lf)
# }

log_factorial = function(logFactorialSheet, n, base) {
  if (n < 100) {
    lf = log(factorial(n))
  } else {
    lf = n * log(n, base) - n * log(exp(1), base) + 0.5 * (log(n, base) + log(2 * pi, base))  
  }
  return(lf)
}