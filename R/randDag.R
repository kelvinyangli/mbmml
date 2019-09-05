#' A function to generate a random DAG
#'
#' This function randomly generate a DAG based on the specified structure details. Not all pairs of variables are
#' connected in the resulting DAG, especially for small number of maxNPas.
#' @param nvars The desired number of varaibles.
#' @param maxNPa The desired maximum number of parents, with minimum 0 parent.
#' @return The output of this function is a DAG in the bnlearn format.
#' @export
randDag = function(nvars, maxNPas) {

  vars = paste0("V", 1:nvars)

  dag = empty.graph(vars)

  if (maxNPas > 0) { # if nodes have parents, sample parents from preceding nodes

    for (i in 2:length(vars)) {

      nPas = sample(0:min(maxNPas, i - 1), 1)

      parents = sample(vars[1:(i - 1)], nPas)

      if (length(parents) > 0) { # add arc only if a node has at least 1 parents

        for (j in 1:nPas) dag = set.arc(dag, parents[j], vars[i])

      }

    } # end for i

  } # else return empty dag

  # re-order vars to keep the ordering consistent when generating cpts and data
  nodes(dag) = node.ordering(dag)
  return(dag)

}
