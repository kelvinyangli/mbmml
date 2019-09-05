#' Convert sll MB output to a list of MBs
#'
#' This is a function to convert the learned MB from sll, stored in a text file
#' to a list of MBs for easy evaluation.
#' @param filePath the full path of a sll MB output file.
#' @param vars A vector of variables. The order is consistent with the colnames
#' of the data set used by sll.
#' @export

sll2list = function(filePath, vars) {
  res_sll = read.table(filePath, sep = "\n")
  mbl_sll = list()
  for (i in 1:length(vars)) {
    temp = strsplit(as.character(res_sll[i, 1]), ": ")[[1]][-1]
    if (length(temp) > 0) {
      ind = as.numeric(strsplit(temp, " ")[[1]])
      mbl_sll[[i]] = vars[ind + 1]
    } else {
      mbl_sll[[i]] = vector(length = 0)
    }
  }
  names(mbl_sll) = vars
  return(mbl_sll)
}
