#' An auxiliary function for mml_pt()
#'
#' Given a node x and its learned Markov blanket mb(x), this function pre-calculates the mml score of each node 
#' y in mb(x) given its potential parents. There are certain nodes that can't be parents of y, such as any 
#' subset that contains y, or any subset that doesn't contain the target node x, because if x is not a 
#' parent of y, then any parents of y must not exist in mb(x). This function helps speed up computing mmlcpt 
#' for an entire mbpt.
#' @param vars A vector of all variables. 
#' @param learnedMB The learned mb of the target node 
#' @param target A taregt node. 
#' @export
mml_cpt_cach = function(indexListPerNodePerValue, arities, sampleSize, vars, target, learnedMB) {
  
  # generate a matrix to store mml score for each node given eligible parents 
  mmlMatrix = matrix(0, nrow = length(learnedMB) + 1, ncol = 2 ^ (length(learnedMB) + 1) - 1)
  if (length(learnedMB) == 0) {
    
    dimnames(mmlMatrix) = list(target, "NULL")
    
  } else {
    
    dimnames(mmlMatrix) = list(c(target, learnedMB), powerset(c(target, learnedMB)))
    
  }

  for (i in 1:nrow(mmlMatrix)) {
    
    var = rownames(mmlMatrix)[i]
    varIndex = which(vars == var)
    
    # when there is no parent
    mmlMatrix[i, 1] = mml_cpt(indexListPerNodePerValue, arities, sampleSize, c(), varIndex, base = exp(1))
    if (length(learnedMB) > 0) {
      
      for (j in 2:ncol(mmlMatrix)) {
        
        pa = colnames(mmlMatrix)[j]
        pa = strsplit(pa, "_")[[1]]
        parentsIndices = which(vars %in% pa)
        
        if (!var %in% pa) {# when pa(var) doesn't involve itself
          
          if ((var == target) || (target %in% pa)) {
            
            mmlMatrix[i, j] = mml_cpt(indexListPerNodePerValue, arities, sampleSize, parentsIndices, varIndex, base = exp(1))
          } # end if 
          
        } # end if 
        
      } # end for j 
      
    } # end if 
    
  } # end for each var i 
  
  return(mmlMatrix)
  
}
