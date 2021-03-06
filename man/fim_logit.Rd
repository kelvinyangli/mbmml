% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fim_logit.R
\name{fim_logit}
\alias{fim_logit}
\title{A function to calculate FIM entries}
\usage{
fim_logit(dataNumeric, betaDotX, xIndices, yIndex)
}
\arguments{
\item{dataNumeric}{Numerical format of original data, where variable values start from 0.}

\item{betaDotX}{Inner product of parameters beta and input variables X.}

\item{xIndices}{A vector of indices of the input variables.}

\item{y}{The index of the output/target variable.}
}
\description{
This function calculates the finsher information matrix of a 1st order logit model.
}
\details{
FIM is a square matrix with dimensions equal to |beta| = |xIndices| + 1 for binary logit models.
rowID and colID in FIM corresponds to rowID-1 and colID-1 in xIndices, because the first row and column 
of FIM is the 2nd derivative w.r.t. beta_0. There is no need to add a column of 1s in front of 
dataNumeric, because the function start filling in entries from 2nd row and 2nd column. The 1st row and 
column are filled in separately.
}
