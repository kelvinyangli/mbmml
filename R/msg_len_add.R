#' Auxiliary function to msg_len_ave()
#'
#' This function uses message lengths to calculate the weighted averaged probability and returns an averaged 
#' message length based on this weighted averaged probability. 
#' @param x The message length of one model. 
#' @param y The message length of another model. 
#' @return The function outputs the averaged message length in natural log. 
#' @export
msg_len_add = function(x, y) {
  
  if ((y - x > 0) && (y - x < 20)) {
    
    l = x - log(1 + exp(x - y))
    
  } else if ((x - y > 0) && (x - y < 20)) {
    
    l = y - log(1 + exp(y - x))
    
  } else if (y - x >= 20) {
    
    l = x
    
  } else if (x - y >= 20) {
    
    l = y
    
  } else if (x == y) {
    
    l = x - log(2)
    
  }
  
  return(l)
  
}