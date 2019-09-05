#' A function to enforce the symmetry correction after Markov blankets have been learned
#'
#' This function does the same enforcement as symmetry_correction, but the MBs are
#' saved in adj mtx.
#' @param mtx Adjacency matrix that stores the learned MBs.
#' @param rule A string argument takes either "union" or "intersection",
#' indicating the two deterministic rules.
#' @export
symmetry_correction_adj_mtx = function(mtx, rule) {

  for (i in 1:(nrow(mtx) - 1)) {
    for (j in (i + 1):ncol(mtx)) {
      if (mtx[i, j] != mtx[j, i]) {
        if (rule == "union") {
          mtx[i, j] = mtx[j, i] = 1
        } else if (rule == "intersection") {
          mtx[i, j] = mtx[j, i] = 0
        }
      }
    }
  }

  return(mtx)
}

