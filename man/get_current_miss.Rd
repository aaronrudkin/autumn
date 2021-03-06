% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/current_miss.R
\name{get_current_miss}
\alias{get_current_miss}
\title{Calculate current errors across variables}
\usage{
get_current_miss(data, target, weights = rep(1, nrow(data)),
  error_function = current_miss_linear)
}
\arguments{
\item{data}{A data frame (tibble) or matrix containing the variables being
targeted. Additional variables can be present in \code{data}, but no
variables in \code{target} should be missing.}

\item{target}{As with \code{\link{harvest}}, this can be either a list of
named vectors, where each vector contains the levels of one variable and
the names of the list are the names of the variables; or else a data frame
(tibble) supplying in order the variable, level, and proportion for the
target}

\item{weights}{The current weights; if left blank, all units will be
assigned weight 1 and this will show imbalance in the original sample.}

\item{error_function}{Specification of error function (how we measure how
far off a variable is from its intended result). Valid
\code{error_function} choices are "linear" (default), "max", "squared",
"mean", "maxsquared", and "meansquared". Users can also supply a closure
(unquoted name of a function). For documentation about error functions,
see \code{\link{harvest}}.}
}
\value{
A named vector containing the degree of error, whose names are the
  variables supplied in the order they were present in \code{target}
}
\description{
This function calculates the extent to which the data under the current
weighting scheme diverges from the target proportions, according to any
error function.
}
\examples{
\dontrun{
# Can be used for data without weights to see initial error
get_current_miss(respondent_data, ns_target)

# Supply weights
result <- harvest(respondent_data, ns_target)
get_current_miss(respondent_data, ns_target, result$weights)

# By default, the error function adds the linear errors across levels of
# the target proportions.
get_current_miss(respondent_data, ns_target, error_function = "max")
}
}
