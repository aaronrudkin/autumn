check_any_startup_issues = function(data, weights, convergence, ...) {
  if(any(!c("pct", "absolute") %in% names(convergence)) ||
     any(convergence[c("pct", "absolute")] < 0) ||
     any(convergence[c("pct", "absolute")] > 1)) {
    stop("The supplied `convergence` values are invalid. `convergence` must ",
         "be a named vector containing the names `pct` and `absolute`, both ",
         "of which are positive and less than 1")
  }

  if("time" %in% names(convergence) &&
     !is.null(convergence[["time"]]) && convergence[["time"]] < 0) {
    stop("If `time` is supplied to `convergence` argument, it must be non-",
         "negative. Remove `time` or update it to be a positive value.")
  }

  if("single_weight" %in% names(convergence) &&
     convergence[["single_weight"]] < 0) {
    stop("If `single_weight` is supplied to `convergence` argument, it must ",
         "be non-negative. Remove `single_weight` or update it to be a ",
         "positive value.")
  }

  if(!is.data.frame(data) && !is.matrix(data)) {
    stop("The supplied `data` argument must be a data frame (tibble) or ",
         "matrix. Instead, it is a ", class(data), ".")
  }

  extra_args = list(...)
  if(length(extra_args)) {
    extra_args_text = paste0(names(extra_args), collapse = ", ")
    message("Note: Function call included unexpected extra arguments (",
            extra_args_text, "). These arguments will be ignored. ")
  }
}

check_any_data_issues = function(data, target, weights) {
  # Basic audit of data issues.

  # First up: Are there any variables in the target that are not in the data?
  columns = names(target)
  if(any(!columns %in% colnames(data))) {
    missing_cols = columns[which(!columns %in% colnames(data))]
    mc_text = paste0(missing_cols, collapse = ", ")
    grammar_text = ifelse(length(missing_cols) > 1, "Variables", "Variable")
    stop("Error raking: ", grammar_text, " missing from data: ",
         mc_text)
  }

  # Second up, are there any levels in the target target that are not in
  # the data?
  sub = data[, columns]
  target_errors = lapply(columns, function(column) {
    missing = setdiff(unique(data[[column]]), c(NA, names(target[[column]])))
    if(!length(missing)) {
      return(NULL)
    }
    paste0("Variable `", column,
           "` missing levels in target which are present in data: ",
           paste0(missing, collapse = ", "))
  })
  target_errors = paste0(unlist(target_errors), collapse = "\n")

  if(nchar(target_errors) > 0) {
    stop("Errors detected in data. Some variables have values in the data ",
         "which are not present in the weight targets:\n",
         target_errors)
  }

  # Are there any levels in the weighting variables in the data that are not
  # in the target target?
  data_errors = lapply(columns, function(column) {
    missing = setdiff(names(target[[column]]), unique(data[[column]]))
    if(!length(missing)) {
      return(NULL)
    }
    paste0("Variable `", column, "` missing levels in data: ",
           paste0(missing, collapse = ", "))
  })
  data_errors = paste0(unlist(data_errors), collapse = "\n")
  if(nchar(data_errors) > 0) {
    stop("Errors detected in data. Some variables have values in the weight ",
         "targets which are not present in the data:\n",
         target_errors)
  }

  # Do any variables in the target weights not sum to 1?
  weight_sum_errors = lapply(names(target), function(variable) {
    sum_target = sum(target[[variable]])
    if(all.equal(sum_target,1) ) {
      return(NULL)
    }

    paste0("Target variable `", variable, "` has targets that do not sum ",
           "to 1.")
  })
  weight_sum_errors = paste0(unlist(weight_sum_errors), collapse = "\n")
  if(nchar(weight_sum_errors) > 0) {
    stop("Errors detected in weight targets:\n", weight_sum_errors)
  }

  # Do any target variables have any negative weight levels?
  neg_weight_errors = lapply(names(target), function(variable) {
    neg_target = target[[variable]] < 0
    if(any(neg_target)) {
     return(
       paste0("Target variable `", variable, "` contains a negative weight.")
     )
    }
    return(NULL)
  })
  neg_weight_errors = paste0(unlist(neg_weight_errors)[1:10], collapse = "\n")
  if(nchar(neg_weight_errors) > 0) {
    stop("Errors detected in weight targets:\n", neg_weight_errors)
  }

  if(any(weights < 0)) {
    stop("Error: Starting weights include one or more weights below 0.")
  }

  if(length(weights) != nrow(data)) {
    stop("Error: Length of provided `weights` argument is not equal to ",
         "number of rows in `data`.")
  }
}

generate_start_weights = function(data, start_weights, max_weight) {
  # Most likely case: start_weight = 1. Of course, if you put
  # start_weights = (n != 1), then it'll get averaged to 1 in a second
  # anyway
  if(length(start_weights) == 1) {
    return(rep(start_weights, nrow(data)))
  }

  # One weight for every observation
  if(nrow(data) != length(start_weights)) {
    stop("`start_weights` must be a single number or a vector of length ",
         nrow(data), " (the number of observations in the data).")
  }

  # Can't work with NA values
  if(any(is.na(start_weights))) {
    stop("`start_weights` must not contain any NA values.")
  }

  # You doofus, you made a weight higher than the maximum
  if(any(start_weights > max_weight)) {
    warning("One or more start weights are above the maximum weight value ",
            "and will be clamped as soon as possible.")
  }

  return(start_weights)
}

get_error_function = function(error_function) {
  # User supplied a string as an error function
  if(is.character(error_function)) {
    map_errors = list("total" = current_miss_linear,
                      "max" = current_miss_max,
                      "squared" = current_miss_squared,
                      "linear" = current_miss_linear,
                      "mean" = current_miss_mean,
                      "average" = current_miss_mean,
                      "totalsquared" = current_miss_squared,
                      "maxsquared" = current_miss_max_squared,
                      "meansquared" = current_miss_mean_squared,
                      "averagesquared" = current_miss_mean_squared)
    if(error_function %in% names(map_errors)) {
      return(map_errors[[error_function]])
    }

    warning("The specified error function '", error_function, "' is not a ",
            "valid option. Valid options are 'linear', 'max', 'squared', ",
            "'total','mean', 'totalsquared', 'maxsquared', 'meansquared'. ",
            "Analysis proceeding using 'linear' error function.")
    return(current_miss_linear)
  }

  # User didn't supply a closure
  if(class(error_function) != "function" ||
     typeof(error_function) != "closure") {
    stop("Error: Invalid option supplied for `error_function`. You must ",
         "supply either a function closure (the unquoted name of a function) ",
         "which takes two arguments and returns a number, or also a quoted ",
         "character string describing a built-in error function. The default ",
         "error function is 'linear'.")
  }

  function_arguments = formals(error_function)
  if(length(function_arguments) > 2) {
    warning("The supplied `error_function` takes more than two arguments. ",
            "`error_function` should be a function that takes two arguments ",
            "exactly and returns a single scalar. Proceeding to use error ",
            "function, but behavior may be unreliable.")
  } else if(length(function_arguments) < 2 &&
            names(function_arguments)[1] != "...") {
    stop("The supplied `error_function` takes too few arguments. ",
         "`error_function` should take exactly two arguments: the target ",
         "values and the current values of a single variable, and return a ",
         "single scalar. You can also omit `error_function` or use a default ",
         "value like 'linear'.")
  }

  return(error_function)
}

check_select_params = function(select_function, select_params) {
  # Mapping from select functions to the parameters required.
  params_req = list(
    "pct" = "pct",
    "number" = "count",
    "lesser" = c("pct", "count"),
    "greater" = c("pct", "count"),
    "nlim" = "count",
    "pctlim" = "pct",
    "nmin" = c("pct", "count"),
    "nmax" = c("pct", "count")
  )

  if(!select_function %in% names(params_req)) return()
  if(all(params_req[[select_function]] %in% names(select_params))) return()

  stop("Error: The supplied variable selection function, \"", select_function,
       "\", requires the following parameter(s): ",
       paste0(params_req[[select_function]], collapse = ", "),
       " but one or more are missing. Specify these parameters or choose a ",
       "different selection function, like \"all\" which does not require ",
       "parameters.")
}

get_select_function = function(select_function, select_params) {
  # User supplied a string as an select function
  if(is.character(select_function)) {
    map_select = list("pct" = select_pct,
                      "number" = select_number,
                      "all" = select_all,
                      "lesser" = select_lesser,
                      "greater" = select_greater,
                      "nolim" = select_all,
                      "nlim" = select_number,
                      "pctlim" = select_pct,
                      "nmin" = select_greater,
                      "nmax" = select_lesser)
    if(select_function %in% names(map_select)) {
      check_select_params(select_function, select_params)
      return(map_select[[select_function]])
    }

    warning("The specified selection function '", select_function, "' is not ",
            "a valid option. Valid options are 'pct, 'number', 'all', ",
            "'lesser', 'greater'. Analysis proceeding using 'pct' select ",
            "function.")
    check_select_params("pct", select_params)
    return(select_pct)
  }

  # User didn't supply a closure
  if(class(select_function) != "function" ||
     typeof(select_function) != "closure") {
    stop("Error: Invalid option supplied for `select_function`. You must ",
         "supply either a function closure (the unquoted name of a function) ",
         "which takes two arguments (a named vector of variable errors and a ",
         "named vector of parameters) and returns a subset of the first ",
         "argument, or a quoted character string describing a built-in ",
         "selection function. The default selection function is 'pct'.")
  }

  function_arguments = formals(select_function)
  if(length(function_arguments) > 2) {
    warning("The supplied `select_function` takes more than two arguments. ",
            "`select_function` should be a function that takes two arguments ",
            "exactly and returns a named numeric vector. Proceeding to use ",
            "select function, but behavior may be unreliable.")
  } else if(length(function_arguments) < 2 &&
            names(function_arguments)[1] != "...") {
    stop("The supplied `select_function` takes too few arguments. ",
         "`select_function` should take exactly two arguments: a named vector ",
         "of variable errors and a named vector of parameters, and return a ",
         "subset of the first argument. You can also omit `select_function` ",
         "or use a default value like 'pct'")
  }

  return(select_function)
}

name_weight_column = function(data, override = NULL) {
  if(!is.null(override)) {
    return(override)
  }

  if(!"weights" %in% colnames(data)) {
    return("weights")
  }

  if(!".weights_autumn" %in% colnames(data)) {
    message("Note: Because the column 'weights' already exists in supplied ",
            "frame, the weights are being stapled onto the data frame as ",
            "'.weights_autumn'.")
    return(".weights_autumn")
  }

  for(i in 1:10) {
    if(!paste0(".weights_autumn", i) %in% colnames(data)) {
      message("Note: Because weights columns already exist in data, the ",
              "weights are being stapled onto the data as '.weights_autumn",
              i, "'.")
      return(paste0(".weights_autumn", i))
    }
  }

  stop("Error stapling weights column to data, because columns 'weights', ",
       "'.weights_autumn', and '.weights_autumn1:.weights_autumn10' already ",
       "exist. Please manually specify a `weight_column` argument or return ",
       "weights without attaching to data frame.")
}

#' Pretty print helper for messaging named vectors
#'
#' R's built-in \code{warning()} and \code{message()} functions eat named
#' vectors. Thus, to be able to output the \code{print.default()} version of a
#' named vector to theconsole, it's necessary to use a pretty print helper
#' like this one.
#'
#' @param named_vector A named numeric vector
#' @return A character vector to be used with message() or warning(),
#'   consisting of a column aligned table of names and values mimmicking the
#'   print.default() behaviour for named vectors.
#' @keywords internal
pretty_print_helper = function(named_vector) {
  # Hwo long is the longest variable? What about the console output?
  max_name_length = max(nchar(names(named_vector)))
  text_width = getOption("width")

  # Let's figure out columns, rows, and our output length.
  number_of_columns = floor(text_width / (max_name_length + 1))
  number_of_rows = ceiling(length(named_vector) / number_of_columns)
  format_string = paste0("%", max_name_length, "s")

  # Which row will a variable be in
  row_indices = rep(1:number_of_rows,
                    each=number_of_columns)[1:length(named_vector)]

  # Map each set of variables which make up a row.
  each_row = vapply(
    split(1:length(named_vector), row_indices),
    function(index) {
      # Get the names and values
      row_names = names(named_vector)[index]
      row_values = as.character(round(unname(named_vector[index]), 5))

      # Row name row
      name_row = paste0(paste0(
        sprintf(format_string, row_names),
        collapse = " "), "\n")

      # Row value row
      value_row = paste0(paste0(
        sprintf(format_string, row_values),
        collapse = " "), "\n")

      # Combine the two
      paste0(name_row, value_row)
    }, "")

  # Combine across each set of variables.
  paste0(each_row, collapse="")
}
