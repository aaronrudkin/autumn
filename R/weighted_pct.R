#' Weighted proportion table
#'
#' This is a fast, dependency-free weighted proportion table for 1D variables.
#'
#' @param x A vector of any type
#' @param weights A numeric vector of weights, equal in length to \code{x}
#' @return A named vector with weighted proportions.
#' @export
#' @examples
#' # Setup examle data
#' x <- c(rep(1, 5), rep(2, 5))
#' weights <- c(rep(1, 5), rep(2, 5))
#'
#' # Unweighted proportions: 0.5 / 0.5
#' prop.table(table(x))
#' # Weighted proportions: 0.33 / 0.66
#' weighted_pct(x, weights)
weighted_pct = function(x, weights) {
  # Very quick weighted table with no external dependencies.
  # Split the current weights into categories of x; sum the weights; and
  # divide by the total weights in the population. Why vapply? Slightly
  # faster than sapply, since R doesn't need to worry about the types.
  vapply(split(weights, x), sum, 0, na.rm = TRUE) /
    sum(weights, na.rm = TRUE)
}
