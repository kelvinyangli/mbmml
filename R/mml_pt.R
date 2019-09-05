#' A function to calculate the mml score of a polytree
#'
#' This function calculates the mml score of a polytree. It searches through the space of all possible polytrees using 
#' mml_cpt as the objective function. 
#' @param mmlCPTCach A matrix of pre-calculated mml score of each node in a mb given its possible parents. This is done
#' using the mml_cpt_cach() function.
#' @param mbptsList A list of all possible mbpts from mb size 0 up to 7. 
#' @param target A taregt node. 
#' @param learnedMB The learned mb of the target node. 
#' @export
mml_pt = function(mmlCPTCach, mbptsList, target, learnedMB) {
  mbpts = mbptsList[[length(learnedMB) + 1]]
  mbpts = substitute_vars(mbpts, target, learnedMB)
  mmlMin = Inf
  index = 0 
  for (j in 1:length(mbpts)) {
    mmlCurrent = mml_dag_fast(mbpts[[j]], mmlCPTCach)
    #cat(mmlCurrent, "\n")
    if (mmlCurrent < mmlMin) { # if mbpt[j] is better then replace mml_min and its index
      mmlMin = mmlCurrent
      index = j
    } 
  }
  return(mbpts[[index]]) # return the optimal local str for each var
}


