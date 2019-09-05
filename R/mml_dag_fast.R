#' A function to calculate the mml score of a dag
#'
#' This function calculates the mml score of a dag. The mml score of a dag is the sum of the mml score of a node given
#' its parents in the dag. 
#' @param adjmtx The adjacency matrix of a given dag. 
#' @param mmlCPTCach The pre-calculated mml score of a node given its potential parents. 
#' @export
mml_dag_fast = function(adjmtx, mmlCPTCach) {
  
  mml = 0 
  
  for (j in 1:ncol(adjmtx)) {
    
    var = colnames(adjmtx)[j]
    pa = colnames(adjmtx)[which(adjmtx[, j] == 1)]
    
    if (length(pa) == 0) {# when there are no paretns
      
      mml = mml + mmlCPTCach[var, "NULL"]
      
    } else {
      
      pa = pa[order(pa)]
      pa = paste(pa, collapse = "_")
      mml = mml + mmlCPTCach[var, pa]
      
    } # end else 
    
  } # end for j
  
  return(mml)
  
}