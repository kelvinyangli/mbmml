#' A function to generate random adjacency matrix
#'
#' This function randomly generate a DAG based on the specified structure details. It is the same as the 
#' randDag() function, except the generated DAG is stored in its adjacency matrix format. The adjacency
#' matrix can be transferred into the bnlearn format DAG using the matrix2dag() function. 
#' @param nvars The desired number of varaibles. 
#' @param maxNPa The desired maximum number of parents, with minimum 0 parent. 
#' @param adjList A list of user preferred adjacencies. The default is NULL, meaning no preferred adjacenceis. 
#' @export
randAdjmtx = function(nvars, maxNPa, adjList = NULL) {
  
  vars = paste0("V", 1:nvars)
  mtx = matrix(0, nvars, nvars, dimnames = list(vars, vars))
  
  if (is.null(adjList)) {
    
    for (i in 2:nvars) {
      
      nPa = sample(0:min(maxNPa, i - 1), 1) 
      
      if (nPa > 0) { # add arc only if a node has at least 1 parents
        
        pa = sample(1:(i - 1), nPa)
        for (j in 1:nPa) mtx[pa[j], i] = 1
        
      } # end if 
      
    } # end for i
    
  } else {
    
    if (length(adjList) > 0) {
      
      for (i in 1:length(adjList)) mtx[adjList[[i]][1], adjList[[i]][2]] = 1
      
      if (max(apply(mtx, 1, sum)) > maxNPa) cat("The maxNPa is exceeded!")
      if (!isDag(mtx)) cat("The adj mtx contains cycles!")
      
    } # end if length(adjList) > 0
    
  } # end else 
  
  return(mtx)
  
}