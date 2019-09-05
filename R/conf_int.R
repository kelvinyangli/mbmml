#' A function to calculate the confidence interval 
#'
#' This is a function to calculate the confidence interval of a given vector of statistics. The default coefficient 
#' 1.96. 
#' @param v A vector of statistics. 
#' @param coefficient Default value is 1.96.  
#' @export
conf_int = function(v, coefficient = 1.96) {
  
  avg = mean(v)
  ci = coefficient * sd(v) / sqrt(length(v))
  return(c(avg, ci))
  
}