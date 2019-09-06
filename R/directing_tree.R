#' Directing a tree
#'
#' This function gives directions to a tree. It requires a root to be specified 
#' by users.
#' @param tree A tree that is saved in its adjacency matrix format. 
#' @param root A user specified root. 
#' @export
directing_tree = function(tree, root) {
  nbrs = bnlearn::nbr(tree, root)
  pa = bnlearn::parents(tree, root)
  undirectedNbrs = nbrs[!nbrs %in% pa]
  if (length(undirectedNbrs) > 0) {
    for (y in undirectedNbrs) {
      tree = bnlearn::set.arc(tree, root, y)
      tree = directing_tree(tree, y)
    }
  }

  return(tree)
}

