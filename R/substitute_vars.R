#' An auxiliary function for mml_pt()
#'
#' This function replaces variable names in the pre-saved mbpts with the new variable names provided.
#' @param ls A list of pre-saved mbpts with variables v_1, ..., v_n. 
#' @param x The target node. 
#' @param z A vector of new variable names.  
#' @export
substitute_vars = function(ls, x, z) {
  
  xIndex = ncol(ls[[1]])
  
  for (i in 1:length(ls)) {
    
    #nodes(ls[[i]])[nodes(ls[[i]]) != y][order(nodes(ls[[i]])[nodes(ls[[i]]) != y])] = z
    colnames(ls[[i]])[-xIndex][order(colnames(ls[[i]])[-xIndex])] = z
    colnames(ls[[i]])[xIndex] = x
    rownames(ls[[i]]) = colnames(ls[[i]])
    
  } # end for i 
  
  return(ls)
  
}