% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.r
\name{evaluate_itr}
\alias{evaluate_itr}
\title{Evaluate ITR}
\usage{
evaluate_itr(
  fit = NULL,
  user_itr = NULL,
  outcome = c(),
  treatment = c(),
  data = list(),
  budget = 1,
  ngates = 5,
  ...
)
}
\arguments{
\item{fit}{Fitted model. Usually an output from \code{estimate_itr}}

\item{user_itr}{A user-defined function to create an ITR. The function should take the data as input and return an unit-level continuous score for treatment assignment. We assume those that have score less than 0 should not have treatment. The default is \code{NULL}, which means the ITR will be estimated from the \code{estimate_itr}.}

\item{outcome}{A character string of the outcome variable name.}

\item{treatment}{A character string of the treatment variable name.}

\item{data}{A data frame containing the variables specified in \code{outcome}, \code{treatment}, and \code{tau}.}

\item{budget}{The maximum percentage of population that can be treated under the budget constraint.}

\item{ngates}{The number of gates to use for the ITR. The default is 5.
A user-defined function to create an ITR. The function should take the data as input and return an ITR. The output is a vector of the unit-level binary treatment that would have been assigned by the individualized treatment rule. The default is \code{NULL}, which means the ITR will be estimated from the \code{estimate_itr}.
See \code{?evaluate_itr} for an example.}

\item{...}{Further arguments passed to the function.}
}
\value{
An object of \code{itr} class
}
\description{
Evaluate ITR
}
