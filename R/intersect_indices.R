#' Auxiliary function to mml_with_parents()
#'
#' This is an auxiliary function to mml_with_parents() to calculate the intersection indices. 
#' @param numParents numParents
#' @param parentsIndecies parentsIndecies 
#' @param indexListPerNodePerValue indexListPerNodePerValue 
#' @param potentialCombination potentialCombination
#' @export
intersect_indices = function(numParents, parentsIndecies, indexListPerNodePerValue, potentialCombination) {
  
  commonIndecies = indexListPerNodePerValue[[parentsIndecies[1]]][[potentialCombination[1]]]
  
  for (i in 2:numParents) {
    
    commonIndecies = intersect(commonIndecies, indexListPerNodePerValue[[parentsIndecies[i]]][[potentialCombination[i]]])
    
  }
  
  return(commonIndecies)
  
}

