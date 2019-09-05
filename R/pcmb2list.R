#' Convert pcmb's mb results to a list
#'
#' This function converts the MB output of pcmb and iamb (in c++) to a list. PCMB
#' outputs MB results on screen when calling sysmtem() in R.
#' @param output The screen output of pcmb/iamb.
#' @param vars A vector of variables.
#' @param method Either "pcmb" or "iamb".
#' @param n Total number of edges could be safely removed. n > 1.
#' @export

pcmb2list = function(output, vars, method) {
  nvars = length(vars)
  lst = list()
  for (i in 1:nvars) {
    temp = output[i + 1]
    temp = strsplit(temp, "mb:")[[1]][2]
    if (method == "iamb") temp = strsplit(temp, split = " nodes")[[1]][1] # remove string after mb results
    if (!is.na(temp)) {
      cha = strsplit(temp, ",")[[1]]
      # cha = cha[-length(cha)]
      lst[[i]] = vars[as.numeric(cha) + 1]
    } else {
      lst[[i]] = as.character(c())
    }
  }
  names(lst) = vars
  return(lst)
}
