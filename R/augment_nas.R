#' Adjust target proportions to add a category for missing data
#'
#' This function adds an additional level to any target variable to represent
#' missing data. The proportion given to the missing data is the proportion
#' missing in the observed sample. Other proportions are adjusted accordingly.
#' This is a private function and not intended for end-user use.
#'
#' @param data A data frame (tibble) or matrix containing sample data,
#'   specified as in \code{\link{harvest}}
#' @param target A list of named vectors, specified as in \code{\link{harvest}}
#' @param which_vars A boolean, numeric, or character vector specifying which
#'   variables to adjust in this manner. If boolean, all variables are selected.
#'   If character, those named variables are selected. If numeric, variables
#'   are selected based on numeric position amongst the target variables.
#' @return An augmented target list, where affected variables with observed
#'   missing data in the sample has an additional level named "___NA"
#'   reflecting the proportion of data missing in the sample and other levels
#'   are adjusted accordingly
#' @keywords internal
freeze_na_proportion = function(data, target, which_vars) {
  # Which variables are we freezing
  if(is.logical(which_vars)) which_vars = names(target)
  else if(is.numeric(which_vars)) which_vars = names(target)[[which_vars]]

  for(variable in which_vars) {
    # For this variable what percentage of the sample is missing?
    prop_na = sum(is.na(data[[variable]])) / length(data[[variable]])

    # Nothing missing? Move on
    if(!prop_na) {
      next
    }

    # Multiply every existing weight by this
    target[[variable]] = target[[variable]] * (1 - prop_na)
    # Stick on a proportion to the target that is exactly equal
    target[[variable]]["___NA"] = prop_na
  }

  target
}


#' Internal copy of data to overwrite missing data
#'
#' This function replaces NAs in any columns in order to allow for the strategy
#' described in \code{\link{freeze_na_proportion}}. NA values are replaced with
#' "___NA". No modifications done this way are saved and data returned by
#' \code{\link{harvest}} if \code{attach_weights=TRUE} will not be modified.
#' This is a private function and not intended for end-user use.
#'
#' @param data A data frame (tibble) or matrix containing sample data,
#'   specified as in \code{\link{harvest}}
#' @param target A list of named vectors, specified as in \code{\link{harvest}}
#' @param which_vars A boolean, numeric, or character vector specifying which
#'   variables to adjust in this manner. If boolean, all variables are selected.
#'   If character, those named variables are selected. If numeric, variables
#'   are selected based on numeric position amongst the target variables.
#' @return An augmented version of \code{data} with NAs of affected variables
#'   replaced with "___NA", and the columns subset to those in
#'   \code{target}
#' @keywords internal
update_na_values = function(data, target, which_vars) {
  # Which variables are we freezing
  if(is.logical(which_vars)) which_vars = names(target)
  else if(is.numeric(which_vars)) which_vars = names(target)[[which_vars]]

  for(variable in which_vars) {
    if(!any(is.na(data[[variable]]))) next
    data[[variable]][is.na(data[[variable]])] = "___NA"
    data[[variable]] = factor(data[[variable]])
  }

  data[, names(target)]
}
