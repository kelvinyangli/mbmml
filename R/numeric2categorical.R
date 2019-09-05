#' A function to convert numerical variables to categorical
#'
#' This function converts numerical variables to categorical (nominal or ordinal) and add the unique values
#' as the levels of each variable. This function is used when the input data set is in numerical format, such
#' as the data sets for the standard real world Bayesian networks ASIAN, ALARM, etc.
#' @param data A numerical data set where each variable contains values such as 0, 1, 2, ...
#' @export
numeric2categorical = function(data) {

  for (j in 1:ncol(data)) {

    data[, j] = as.factor(data[, j])

  }

  return(data)

}

