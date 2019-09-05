#' A function to convert a bnlearn format dag to its adjacency matrix
#'
#' This function converts a bnlearn format dag to its adjacency matrix. The dag can be either directed or partially
#' directed.
#' @param dag A dag or partial dag in bnlearn format.
#' @export

dag2matrix = function(dag) {

  vars = bnlearn::nodes(dag)
  nvars = length(vars)

  mtx = matrix(0, nrow = nvars, ncol = nvars, dimnames = list(vars, vars))

  if (nrow(dag$arcs) > 0) {

    if (nvars > 1) {

      for (i in 1:nrow(dag$arcs)) {

        mtx[dag$arcs[i, 1], dag$arcs[i, 2]] = 1

      }

    }

  }

  return(mtx)

}
