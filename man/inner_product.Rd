% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inner_product.R
\name{inner_product}
\alias{inner_product}
\title{Calculate inner product of logit parameters and input variables}
\usage{
inner_product(dataPoint, beta, xIndices)
}
\arguments{
\item{dataPoint}{A single data point. That is, a single row of an entire data set.}

\item{beta}{A vector of logit model parameters.}

\item{xIndices}{Indices of input variables.}
}
\description{
This function calculates the inner product of the logit parameters and the input variables given a single
data point.
}
