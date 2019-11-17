select_pct = function(misses, select_params) {
  # Simple percentage threshold.
  misses[misses > select_params[["pct"]]]
}

select_number = function(misses, select_params) {
  # Sort in a decreasing order of error; select the `count` worse variables,
  # then re-sort back in the original order, and return the subset.
  misses[sort(order(misses, decreasing = TRUE)[1:select_params[["count"]]])]
}

select_all = function(misses, select_params) {
  # Return them all
  misses
}

select_lesser = function(misses, select_params) {
  # Run them both and take the smallest length of variables.
  miss_pct = select_pct(misses, select_params)
  miss_number = select_number(misses, select_params)
  if(length(miss_pct) < length(miss_number)) {
    return(miss_pct)
  }

  return(miss_number)
}

select_greater = function(misses, select_params) {
  # Run them both and take the largest length of variables.
  miss_pct = select_pct(misses, select_params)
  miss_number = select_number(misses, select_params)
  if(length(miss_pct) > length(miss_number)) {
    return(miss_pct)
  }

  return(miss_number)
}

variable_selection = function(data, target, weights,
                              select_function,
                              select_params,
                              error_function,
                              error_none = TRUE) {

  # How are we doing?
  misses = get_current_miss(data, target, weights,
                            error_function)

  # Only take those where we're not doing well
  final_miss = select_function(misses, select_params)

  # We might want to actually complain if there are no variables that require
  # raking
  if(length(final_miss) == 0 && error_none) {
    stop("Weighting unnecessary based on parameters supplied to variable ",
         "selection function and error function. This means that the ",
         "variables in your data are already close enough to your targets to ",
         "not need raking. You can override this by setting more aggressive ",
         "parameters or by selecting a different variable selection function ",
         "including `select_function = \"all\".")
  }

  # Return the names of the variables we care about
  names(final_miss)
}
