% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mml_nb.R
\name{mml_nb}
\alias{mml_nb}
\title{A function to calculate the MML for the Naive Bayes model}
\usage{
mml_nb(data, probSign, vars, arities, sampleSize, x, y, debug = FALSE)
}
\arguments{
\item{data}{A given data set.}

\item{probSign}{A data frame with 1 and -1, which corresponds to the 1st and 2nd level of a varaible. 
It is used for computing the FIM of Naive Bayes.}

\item{vars}{A vector of all variables.}

\item{arities}{A vector of variables arities.}

\item{sampleSize}{Sample size of a given data set.}

\item{x}{A vector of input variables.}

\item{y}{The output variable.}

\item{debug}{A boolean variable to display each part of the MML score.}
}
\description{
This function calculates the total MML score of the Naive Bayes model. It is derived from the general 
MML87 formula, but for Naive Bayes only. The parameters used are not MML estimations but maximum 
likelihood estimations due to simplicity. The current version only works with binary variables. The free 
parameter of a variable is the first value (or level) appeared in all values (or levels) sorted by 
alphabetic order.
}
