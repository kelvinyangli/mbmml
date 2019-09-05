#' A function to calculate the edit distance from the learned to the true DAG
#'
#' The true and learned DAGs must be stored as adjacent matrix. The output is a
#' list consists of false positive, false negative, precision, recall and edit
#' distance. Edit distance between two DAGs is defined as the minimum number of
#' arc deletion and addition to turn dagL to dagT.
#' @param dagL The learned DAG.
#' @param dagT The true DAG.
#' @export
edit_dist_dag = function(dagL, dagT) {
  tp = tn = fp = fn = 0
  for (i in 1:nrow(dagL)) {
    for (j in 1:ncol(dagL)) {
      if ((dagL[i, j] == 0) && (dagT[i, j] == 0)) {
        tn = tn + 1
      } else if ((dagL[i, j] == 1) && (dagT[i, j] == 1)) {
        tp = tp + 1
      } else if ((dagL[i, j] == 1) && (dagT[i, j] == 0)) {
        fp = fp + 1
      } else {
        fn = fn + 1
      }
    }
  }
  ed = fp + fn
  pre = tp / (tp + fp)
  rec = tp / (tp + fn)
  ls = list(fp = fp, fn = fn, pre = pre, rec = rec, ed = ed)
  return(ls)
}

