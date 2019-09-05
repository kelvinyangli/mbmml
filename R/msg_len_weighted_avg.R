#' Weighted average message length
#'
#' This function calculates the weighted average over a vector of message lengths. It can either be 
#' uniform or non-uniform. It uses message lengths instead of probabilities to avoid computer under
#' flow when dealing with extreme small probability multiplications. 
#' @param l A vector of more than one message lengths. 
#' @param w A vector of weights, one for each model (structure). It can be a vector of uniform 
#' probabilities that indicates an uniform averaging over all models. 
#' @return The function outputs a weighted average message length in natural log. 
#' @export
msg_len_weighted_avg = function(l, w) {
  
  k = which.min(l)
  lk = l[k]
  wk = w[k]
  indices = c(k, which(l - lk > 19))
  l = l[-indices]
  w = w[-indices]
  avg = lk - log(wk) - log(1 + sum((w / wk) * exp(lk - l)))
  return(avg)
  
}




