#' Random neighbourhood model
#'
#' @param target The target variable.
#' @param nbr nbr
#' @export
rand_nbr_model = function(target, nbr) {

  varOrdering = sample(c(target, nbr))
  str = matrix(0, length(varOrdering), length(varOrdering), dimnames = rep(list(varOrdering), 2))
  ind = which(varOrdering == target)
  if (ind == 1) {

    str[ind, (ind + 1):ncol(str)] = 1

  } else if (ind == ncol(str)) {

    str[1:(ind - 1), ind] = 1

  } else {

    str[ind, (ind + 1):ncol(str)] = 1
    str[1:(ind - 1), ind] = 1

  }

  return(str)

}
