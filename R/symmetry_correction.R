#' A function to enforce the symmetry correction after Markov blankets have been learned
#'
#' This function enforces the symmetry check for each learned mb. That is, if a node x is is mb(y), but y isn't
#' in mb(x), then either add y into mb(x) or delete x from mb(y), depending on which deterministic rule to
#' follow. The two famous deterministic rules are the Intersection and Union rules. For MB discovery using MML, we prefer
#' to use the Intersection rule due to the high precision and low recall of mml_cpt.
#' @param vars A vector of all variables.
#' @param mbList A list of learned Markov blankets.
#' @param rule A string argument takes either "Intersection" or "Union", indicating the two deterministic rules mentioned
#' above.
#' @export
symmetry_correction = function(vars, mbList, rule) {

  for (i in 1:length(vars)) {
    # indices of variables that contain vars[i]
    ind = which(sapply(mbList, is.element, el = vars[i]))
    # indices (from ind above) of variables that are not in mb[i]
    j = ind[which(!vars[ind] %in% mbList[[i]])]

    if (length(j) > 0) {

      if (rule == "union") {
        # if union, then include those that are not in mb[i] but containing vars[i]
        mbList[[i]] = c(mbList[[i]], vars[j])

      } else if (rule == "intersection") {
        # else, don't add them into mb[i] and remove var[i] from their mbs
        mbList[j] = lapply(mbList[j], function(l, x)l = l[l!= x], x = vars[i])

      }

    }

  }

  return(mbList)

}








