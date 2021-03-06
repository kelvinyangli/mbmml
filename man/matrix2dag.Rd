% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix2dag.R
\name{matrix2dag}
\alias{matrix2dag}
\title{A function to convert adjacency matrix to a dag in bnlearn format}
\usage{
matrix2dag(mtx)
}
\arguments{
\item{mtx}{Adjacency matrix}
}
\description{
This function converts an adjacency matrix to a dag in the bnlearn format. The resulting dag could be 
undirected if the learned adjancency matrix is bi-directed.
}
