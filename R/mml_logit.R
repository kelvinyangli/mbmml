#' A function to calculate the mml of the 1st order logit model
#'
#' This function calculates the mml of a 1st order logit model given input and output variables. The mml 
#' 1st order logit formula was derived by [Neil, Wallace and Korb 1999]. The parameter priors are assumed 
#' to be normally distributed with standard deviation sigma = 3. Due to the difficulty of deriving a closed
#' form formula for the determinant of the FIM, the logit parameters are estimated using the glm() function.
#' The estimated parameters are then used to calculate the FIM and its determinant in order to calculate 
#' the mml score. 
#' @param data A categorical data set.
#' @param arities A vector of variable arities in data.
#' @param sampleSize The sample size. That is, the number of rows of data. 
#' @param x A vector of input variables with any length. For an empty input variable, set x = c().
#' @param y The output/target variable. 
#' @param sigma The standard derivation of the assumed Gaussian distribution for parameter prior. The 
#' default value is 3 as suggested by the original paper. 
#' @param debug A boolean argument to display mml score for each part.
#' @return The function by default returns the mml score. But it can also return a list of detailed values, 
#' such as nlogPrior, nlogF, etc. 
#' @export
mml_logit = function(data, arities, sampleSize, x, y, sigma = 3, debug = FALSE) {
  
  yIndex = which(names(data) == y)
  xIndices = which(names(data) %in% x)
  
  # pars
  beta = glm_logit(data, x, y)
  
  if (length(x) < 1) {
    betaDotX = beta # single value 
  } else {
    betaDotX = apply(dataNumeric, 1, inner_product, beta = beta, xIndices = xIndices) # vector  
  }
  # negative log likelihood
  nll = nll_logit(dataNumeric, betaDotX, xIndices, yIndex)
  
  if (length(xIndices) < 1) {# when no parent
    # negative log prior
    nlogPrior = 
      0.5 * log(2 * pi) + log(sigma) - 0.5 * log(arities[yIndex]) + 
      0.5 * beta ^ 2 / sigma ^ 2
    # negative log lattice
    logLattice = 0.5 * (1 + log(0.083333))
    # when no parent, fim = sum_i (exp(betaDotX) / (1 + exp(betaDotX)) ^ 2)
    # log det(fim)
    logF = log((exp(betaDotX) / (1 + exp(betaDotX)) ^ 2) * sampleSize)
  } else {
    nFreePars = length(beta) # number of free parameters
    # negative log prior
    nlogPrior = 
      0.5 * nFreePars * log(2 * pi) + nFreePars * log(sigma) - 0.5 * log(arities[yIndex]) - 
      0.5 * sum((arities[xIndices] - 1) * log(arities[yIndex]) + (arities[yIndex] - 1) *
                  log(arities[xIndices])) + 0.5 * sum(beta ^ 2) / sigma ^ 2
    # lattice constant
    k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
    if (nFreePars <= length(k)) {
      latticeConstant = k[nFreePars]
    } else {
      latticeConstant = min(k)
    }
    # negative log lattice constant
    logLattice = 0.5 * nFreePars * (1 + log(latticeConstant))
    fim = fim_logit(dataNumeric, betaDotX, xIndices, yIndex)
    # log det(fim)
    logF = log_determinant(fim)
  }
  
  # mml score
  mml = nlogPrior + logLattice + 0.5 * logF + nll
  lst = list(nlogPrior + logLattice + 0.5 * logF, mml, nlogPrior, logLattice, logF, nll)
  names(lst) = c("1st", "mml", "nlogPrior", "logLattice", "logF", "nll")
  
  if (debug) {
    cat("mml=", mml, "\n")
    cat("@ 1st part=", mml-nll, "\n")
    cat("*** -logPrior=", nlogPrior, "\n")
    cat("*** detFIM=", exp(logF), "\n")
    cat("*** logFisher=", logF, "\n")
    cat("*** logLattice=", logLattice, "\n")
    cat("@ 2nd part=nll=", nll, "\n")
  }
  
  return(mml)
  
}
