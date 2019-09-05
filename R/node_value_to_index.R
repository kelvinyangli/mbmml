#' An auxiliary function to mml_nb_adaptive()
#'
#' This function converts the variable values to an index in the set of all combinations. For example, the values 
#' c("A","A","A") is converted into 1, c("B","A","A") is converted to 2. 
#' @param arities A vector of arities of the Xs and Y. It is in the order of c(xIndices, yIndex).
#' @param value A vector of variable values in the order of c(xIndices, yIndex). The values are in numeric format.
#' @export
node_value_to_index = function(arities, values) {
  index = values[1]
  for (i in 2:length(values)) {
    index = index + (values[i] - 1) * prod(arities[1:(i - 1)])
  }
  return(index)
}