#' A function to convert adjacency matrix to a dag in bnlearn format
#'
#' This function converts an adjacency matrix to a dag in the bnlearn format. The resulting dag could be 
#' undirected if the learned adjancency matrix is bi-directed.  
#' @param mtx Adjacency matrix 
#' @export
matrix2dag = function(mtx) {
  dag = empty.graph(colnames(mtx)) 
  for (i in 1:nrow(mtx)) {
    for (j in 1:ncol(mtx)) {
      
      if (mtx[i, j] == 1) {
        if (mtx[j, i] == 1) {# if bidirected
          dag = set.edge(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
          mtx[j, i] = 0
        } else {# singly directed
          dag = set.arc(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
        } 
        mtx[i, j] = 0 # remove edge to avoid double counting
      } # end if 
      
    }
  }
  return(dag)
}




