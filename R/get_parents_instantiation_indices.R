#' Auxiliary function to mml_cpt() 
#'
#' @param arities arities
#' @param numParents numParents
#' @param parentsIndices parentsIndices
#' @param numParentsInstantiations numParentsInstantiations
#' @param index index
#' @export
get_parents_instantiation_indices = function(arities, numParents, parentsIndices, numParentsInstantiations, 
                                             index) {
  
  if (index <= numParentsInstantiations) {
    
    indices = rep(0, numParents)
    
    indices[1] = ceiling(index / prod(arities[parentsIndices][2:numParents]))
    
    for (i in 1:(numParents - 1)) {
      
      indicator = ceiling(index / prod(arities[parentsIndices][(i + 1):numParents]))
      
      indices[i] = indicator %% arities[parentsIndices][i]
      
      if (indices[i] == 0) indices[i] = arities[parentsIndices][i]
      
    }
    
    
    lastIndex = index %% arities[parentsIndices][numParents] # take modular 
    
    if (lastIndex == 0) {
      
      indices[numParents] = arities[parentsIndices][numParents]
      
    } else {
      
      indices[numParents] = lastIndex
      
    }
    
    return(indices)
    
  } else {
    
    return(0)
    
  }
  
}