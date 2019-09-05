#' A function to count occurances of variable values
#'
#' This function store the occurance of each value of each node in a list in order to speed up the calculation
#' of mml_cpt. 
#' @param data A categorical (nominal or ordinal) data set.
#' @param arities A list of arities for variables. 
#' @export
count_occurance = function(data, arities) {
  indexListPerNodePerValue = list()
  for (i in 1:ncol(data)) {# get the indecides for each value of node i
    indexListPerValue = list()
    for (j in 1:arities[i]) {
      indexListPerValue[[j]] = which(data[, i] == levels(data[, i])[j]) 
    } 
    indexListPerNodePerValue[[i]] = indexListPerValue
  } 
  return(indexListPerNodePerValue)
}