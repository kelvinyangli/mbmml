#' A function to generate random CPTs for a given DAG
#'
#' This function randomly generate conditional probability tables (CPTs) for a given DAG, with specified
#' maximum arity and concentration parameter from a symmetric Dirichlet distribution. Values of variables
#' are sampled from capital alphabet letters, A, B, ... The function has dependency on the library gtools
#' on its rdirichlet() function.
#' @param dag A DAG in the bnlearn format.
#' @param maxArity The maximum arity for variables. That is, the maximum number of values that a variable
#' can have.
#' @param beta The concentration parameter is a symmetric Dirichlet distribution. When beta=1, it is the
#' same as sampling from uniform distribution; when beta>1, CPT values are more likeliy to be equal since
#' they are sampled from a center-peaked n-1 simplex; when beta<1, CPT values are extreme, since they are
#' sampled from conners of the n-1 simplex.
#' @param arities The vector of arities for all nodes. By default, it is NULL.
#' @param debug A boolean argument to display detailed steps.
#' @return The returned CPTs are stored in a list that matches bnlearn CPTs format, so that the function
#' bnlearn::rbn() can be used to sample data.
#' @export
randCPTs = function(dag, maxArity, beta, arities = NULL, debug = FALSE) {
  # the larger beta is, the more beta the distribution is, hence values are more close to the middle point

  vars = bnlearn::nodes(dag)
  nvars = length(vars)

  if (is.null(arities)) {
    # sample cardinalities for nodes
    if (maxArity == 2) {
      cardinalities = rep(2, nvars)
      beta = rep(beta, 2) # equal beta parameters for all values
    } else {
      cardinalities = sample(2:maxArity, nvars, replace = TRUE)
      beta = rep(beta, max(cardinalities)) # equal beta parameters for all values
    }
  } else { # fixed arities
    cardinalities = arities
    beta = rep(beta, max(cardinalities))
  }


  cpts = list()

  if (debug) cat("* sampling cpt values \n")

  # generate cpts for all nodes
  for (i in 1:nvars) {

    parents = dag$nodes[[i]]$parents

    if (length(parents) < 1) {

      sampledCPT = rdirichlet(1, beta[1:cardinalities[i]]) # sample single cpt from dirichlet

      cpts[[i]] = array(sampledCPT, dim = c(1, cardinalities[i]), dimnames = list(NULL, LETTERS[1:cardinalities[i]]))

    } else {

      parentsIndex = which(vars %in% parents)

      sampledCPT = rdirichlet(prod(cardinalities[parentsIndex]), beta[1:cardinalities[i]]) # sample multiple cpts from dirichlet with the above beta
      sampledCPT = t(sampledCPT) # take the transpose

      dimNames = list(LETTERS[1:cardinalities[i]])

      for (j in 1:length(parents)) {
        dimNames[[j + 1]] = LETTERS[1:cardinalities[parentsIndex[j]]]
      }

      names(dimNames) = c(vars[i], parents)

      cpts[[i]] = array(sampledCPT, dim = c(cardinalities[i], cardinalities[parentsIndex]),
                        dimnames = dimNames)

    }
  }

  names(cpts) = vars

  if (debug) cat("* converting to bn.fit \n")

  bnFit = custom.fit(dag, cpts) # convert cpts into bn.fit format

  return(bnFit)

}
