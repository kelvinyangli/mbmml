#' A function that extract the local structure over a given set of variables from the true DAG
#'
#' @param dag The true DAG.
#' @param varSubset A subset of variables whose local structure we are interested in.
#' @export
local_extraction = function(dag, varSubset) {

  localStr = empty.graph(varSubset)
  for (i in 1:nrow(dag$arcs)) {

    if (prod(dag$arcs[i, ] %in% varSubset) == 1) {

      localStr = set.arc(localStr, dag$arcs[i, 1], dag$arcs[i, 2])

    }

  }

  return(localStr)

}
