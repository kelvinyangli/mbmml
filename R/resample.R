#' A safer version of sample()
#'
#' @param x A vector consists of at least one element.
#' @param ... The same parameters as sample().
#' @export
resample = function(x, ...) {
  
  x[sample.int(length(x), ...)]
  
}