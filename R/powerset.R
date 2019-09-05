#' A function to generate the power set of a set of variables. 
#'
#' This function enumerates the power set of a set of variables except the set of all variables. 
#' @param x A set of variables. 
#' @export
powerset = function(x) {
  
  x = x[order(x)] # re-order vars in x in ascending order for future reference
  pwrset = c("NULL", x) # initialize pwrset with the empty set and subsets with cardi = 1
  
  if (length(x) > 2) {
    
    for (i in 2:(length(x) - 1)) {# for each cardinality 
      
      pwrset = c(pwrset, apply(combn(x, i), 2, paste0, collapse = "_")) # add subsets with cardinality i into pwrset
      
    } # end for i 
    
  } # end if 
  
  #pwrset = c(pwrset, paste0(x, collapse = "_")) # add the subset of all variables at the end
  
  return(pwrset)
  
}