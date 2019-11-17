#' Error Function
#'
#' Returns the error between the current calculated weighted proportions and
#' the target proportions. Method of calculation depends on the error function
#' used. These are private function which is not intended for end-user use.
#'
#' If supplying a custom error function closure to \code{\link{harvest}}, the
#' function should take two arguments: first, the target proportions, and second
#' the current weighted proportions based on the current provisional weights.
#'
#' Error functions:
#' \describe{
#'   \item{\code{current_miss_linear}}{Sum of absolute differences}
#'   \item{\code{current_miss_squared}}{Sum of squared differences}
#'   \item{\code{current_miss_max}}{Maximum absolute difference}
#'   \item{\code{current_miss_mean}}{Mean absolute difference}
#'   \item{\code{current_miss_max_squared}}{Maximum squared difference}
#'   \item{\code{current_miss_mean_squared}}{Mean squared difference}
#' }
#'
#' @param truth A vector containing the true proportions of the target variable
#' @param current A vector containing the weighted proportions of the target
#'   variable based on the current weights.
#' @return The calculated error, a single scalar greater than 0.
#' @keywords internal
current_miss_linear = function(truth, current) {
  # The miss is the sum of absolute difference.
  sum(abs(truth - current))
}

#' @rdname current_miss_linear
current_miss_squared = function(truth, current) {
  sum((truth - current)^2)
}

#' @rdname current_miss_linear
current_miss_mean = function(truth, current) {
  # Why sum / length? It's about twice as fast as taking the mean
  sum(abs(truth - current)) / length(truth)
}

#' @rdname current_miss_linear
current_miss_max = function(truth, current) {
  max(abs(truth - current))
}

#' @rdname current_miss_linear
current_miss_max_squared = function(truth, current) {
  max((truth - current)^2)
}

#' @rdname current_miss_linear
current_miss_mean_squared = function(truth, current) {
  # Why sum / length? It's about twice as fast as taking the mean
  sum((truth - current)^2) / length(truth)
}

#' Calculate current errors across variables
#'
#' This function calculates the extent to which the data under the current
#' weighting scheme diverges from the target proportions, according to any
#' error function.
#'
#' @param data A data frame (tibble) or matrix containing the variables being
#'   targeted. Additional variables can be present in \code{data}, but no
#'   variables in \code{target} should be missing.
#' @param target As with \code{\link{harvest}}, this can be either a list of
#'   named vectors, where each vector contains the levels of one variable and
#'   the names of the list are the names of the variables; or else a data frame
#'   (tibble) supplying in order the variable, level, and proportion for the
#'   target
#' @param weights The current weights; if left blank, all units will be
#'   assigned weight 1 and this will show imbalance in the original sample.
#' @param error_function Specification of error function (how we measure how
#'   far off a variable is from its intended result). Valid
#'   \code{error_function} choices are "linear" (default), "max", "squared",
#'   "mean", "maxsquared", and "meansquared". Users can also supply a closure
#'   (unquoted name of a function). For documentation about error functions,
#'   see \code{\link{harvest}}.
#' @return A named vector containing the degree of error, whose names are the
#'   variables supplied in the order they were present in \code{target}
#' @export
#' @examples
#' \dontrun{
#' # Can be used for data without weights to see initial error
#' get_current_miss(respondent_data, ns_target)
#'
#' # Supply weights
#' result <- harvest(respondent_data, ns_target)
#' get_current_miss(respondent_data, ns_target, result$weights)
#'
#' # By default, the error function adds the linear errors across levels of
#' # the target proportions.
#' get_current_miss(respondent_data, ns_target, error_function = "max")
#' }
get_current_miss = function(data, target, weights=rep(1, nrow(data)),
                            error_function = current_miss_linear) {

  # Lookup error function so we can use string error functions as well as
  # the default closure
  error_function = get_error_function(error_function)

  # For each target weight variable, get the miss.
  misses = vapply(names(target), function(x) {
    # Get current
    current = weighted_pct(data[[x]], weights)

    # error_function is any function that takes two arguments: the true values
    # and the current guess, and returns a scalar summary of the miss across
    # the values. The default is current_miss_linear
    error_function(target[[x]][names(current)], current)
  }, 0)

  # Make sure the variables here are named
  names(misses) = names(target)

  # Return data correctly.
  misses
}
