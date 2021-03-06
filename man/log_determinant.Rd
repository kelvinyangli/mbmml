% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_determinant.R
\name{log_determinant}
\alias{log_determinant}
\title{A function to calculate log of the determinant of a matrix}
\usage{
log_determinant(x)
}
\arguments{
\item{x}{A symmetric positive-definite square matrix.}
}
\description{
This function calculates the log (natural base) of the determinant of a matrix in order to avoid overflow
for some large determinant. The matrix has to be symmetric positive-definite in order to use the 
cholesky decomposition.
}
