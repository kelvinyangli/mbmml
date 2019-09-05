#' Weighted averaged message length
#'
#' This function takes in a vector of message lengths, and returnes an weighted averaged message length. 
#' @param l A vector of message lengths. 
#' @return The function a weighted averaged message length in natrual log.  
#' @keywords This function has dependencies on msg_len_add(). 
#' @export
msg_len_ave = function(l) {
  
  avgL = l[1]
  for (i in 2:length(l)) {
    
    avgL = msg_len_add(avgL, l[i])
    
  }
  
  return(avgL + log(length(l)))
  
}