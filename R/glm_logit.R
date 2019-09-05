#' A function to estimate logit model parameters
#'
#' This function estimates binary logit model parameters using the build in function glm(). The output 
#' variable must be binary, whilst the input variable can be multinomial. 
#' @param data 
#' @param x A vector of input variables with any length. For an empty input variable, set x = c().
#' @param y The output/target variable. 
#' @export
glm_logit = function(data, x, y) {
  if (length(x) < 1) {# no parents
    formula = paste(y, "~ 1")  
  } else {
    formula = paste(y, "~", paste0(x, collapse = "+"))
  }
  # estimate parameter of logit model using glm
  pars = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  names(pars) = c() # get rid of the name for each index
  return(pars)
}