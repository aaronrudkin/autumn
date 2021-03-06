#' Generate iterative raking weights for your data.
#'
#' This function implements a variation of iterative raking, as described in
#' DeBell and Krosnick (2009). It replaces the \code{\link[anesrake]{anesrake}}
#' function in the anesrake package and adds support for modern data types, a
#' tidy workflow, additional user control, and faster estimation.
#' \code{harvest()} is designed so that for most users, the default two-argument
#' function call \code{harvest(data, targets)} will behave well, but almost
#' every element of the process can be customized by users who want additional
#' control.
#'
#' The default parameterization of the function works very well. There should
#' be little need to select the alternate calculation methods or tweak any of
#' the parameters. To the extent that weights do not converge, this is likely
#' to be a pathology of the data or the target proportions rather than the
#' parameterization of iterative raking. Documentation below is primarily
#' intended for advanced users who want to customize parameters in detail.
#'
#' @section Convergence Parameters:
#'
#' By default, \code{harvest()} will return a warning if results do not
#' convergence or only partial convergence is achieved. This normally occurs
#' if rate of convergence slows before weights are stabilized. If this occurs,
#' users can choose between altering parameters to force better convegence,
#' or simply evaluating divergence from population marginals. By default,
#' partial convergence messages appear if the sum of absolute differences in
#' unit weights are changing by more than \code{1e-3} when a breaking rule is
#' triggered. The degree to which this constitutes a meaningful divergence
#' from target proportions is case dependent.
#'
#' The parameter \code{convergence} is a named vector containing three values,
#' \describe{
#'   \item{"pct"}{A threshold governing convergence of the iterative raking
#'     process. Each iteration will typically adjust weights by a smaller
#'     amount than the previous iteration. If the adjustment is greater than
#'     (1 - \code{pct}) of the previous adjustment, convergence is achieved.
#'     In other words, as the magnitude of weight updating becomes stable
#'     across iterations, convergence is achieved. Default 0.01.
#'     \code{\link[anesrake]{anesrake}} uses 0.01. \code{\link[mipfp]{Ipfp}}
#'     and \code{\link[survey]{rake}} do not support this parameter. Values
#'     less than 0 or greater than 1 are not permitted.}
#'   \item{"absolute"}{If the absolute overall weight adjustment between
#'     iterations is less than this parameter, convergence is achieved. Default
#'     1e-6. \code{\link[anesrake]{anesrake}} does not support this parameter.
#'     \code{\link[mipfp]{Ipfp}} uses 1e-10. \code{\link[ipfp]{ipfp}} uses a
#'     machine-dependent parameter approximately equal to 1e-8 on most machines.
#'     }
#'   \item{"time"}{Optional. If provided, runs the iterative raking algorithm
#'     for at most \code{convergence["time"]} seconds. In general, exiting
#'     after a fixed time period will have a negative impact on convergence. If
#'     provided, must be at least 0 and can be fractional (e.g. 0.5 will run
#'     the algorithm for half a second.}
#'   \item{"single_weight"}{If the maximum single weight adjustment between
#'     iterations is less than this parameter, convergence is achieved. Must
#'     be non-negative.}
#' }
#'
#' Users interested in achieving better convergence to target proportions at the
#' cost of time should set the \code{convergence["pct"]} and
#' \code{convergence["absolute"]} to low values (say, \code{0.0001} and
#' \code{1e-8}), and raise \code{max_iterations} as high as possible
#' (say, \code{10,000}). Users interested in quick but imperfect results should
#' use \code{convergence["time"]} to cap runtime.
#'
#' @section Variable Selection Functions and Parameters:
#'
#' Built-in selection functions include:
#' \describe{
#'   \item{"pct"}{Rake on variables whose initial error is greater than
#'     \code{select_params[["pct"]]}. This is the default variable selection
#'     function, and the default \code{select_params[["pct"]]} is 0.05. The
#'     units of \code{select_params[["pct"]]} depend on the error function
#'     selected, but for the default "linear" error function indicate
#'     "no more than 5% absolute deviation across all variable levels,
#'     collectively").}
#'   \item{"all"}{Rake on all variables}
#'   \item{"number"}{Rake on exactly \code{select_params[["count"]]} variables.
#'     The default is 5. Variables are selected in descending order of error.}
#'   \item{"lesser"}{Rake on the smallest set of variables supplied by
#'     the \code{select_params[["pct"]]} and \code{select_params[["count"]]}
#'     arguments.}
#'   \item{"greater"}{Rake on the greater set of variables supplied by
#'     the \code{select_params[["pct"]]} and \code{select_params[["count"]]}
#'     arguments.}
#'   \item{"pctlim"}{Same as "pct", backwards compatibility for anesrake}
#'   \item{"nlim"}{Same as "number", backwards compatibility for anesrake}
#'   \item{"nolim"}{Same as "all", backwards compatibility for anesrake}
#'   \item{"nmin"}{Same as "greater", backwards compatibility for anesrake.
#'     Please note that "nmin" is equivalent to "greater", not "lesser".}
#'   \item{"nmax"}{Same as "lesser", backwards compatibility for anesrake.
#'     Please note that "nmax" is equivalent to "lesser", not "greater".}
#' }
#'
#' Built in \code{select_params} parameters are:
#' \describe{
#'   \item{"pct"}{Percentage threshold used if the selection function is
#'     "threshold", "lesser", or "greater". The scale of this threshold is
#'     total absolute percentage deviation if the linear error function is
#'     used, so \code{pct = 0.05} implies a total deviation from target
#'     proportions of no less than 5% across all levels. For other error
#'     functions, unit scales may differ.}
#'   \item{"count"}{Number of variables to select if the selection function is
#'     "number", "lesser", or "greater". Variables are selected in descending
#'     order of error.}
#' }
#'
#' @section Error Functions:
#'
#' Custom selection functions should take two arguments: a named numeric vector
#' which supplies the available variables and their calculated errors, and a
#' named vector of parameters. Custom selection functions should return a
#' non-empty subset of the named numeric vector.
#'
#' Built-in error functions include:
#' \describe{
#'   \item{"linear"}{Sum of absolute differences}
#'   \item{"squared"}{Sum of squared differences}
#'   \item{"max"}{Maximum absolute difference}
#'   \item{"mean"}{Mean absolute difference}
#'   \item{"maxsquared"}{Maximum squared difference}
#'   \item{"meansquared"}{Mean squared difference}
#'   \item{"total"}{Same as "linear", backwards compatibility for anesrake}
#'   \item{"average"}{Same as "mean", backwards compatibility for anesrake}
#'   \item{"totalsquared"}{Same as "squared", backwards compatibility for
#'     anesrake}
#'   \item{"averagesquared"}{Same as "meansquared", backwards compatibility for
#'     anesrake}
#' }
#'
#' Custom error functions should take two arguments: a numeric vector
#' containing the target proportions, and a numeric vector containing the
#' current weighted performance. They should return a single numeric summary
#' of the data.
#'
#' @section Interpreting \code{NA} Values in Data:
#'
#' If data contains an \code{NA} in raking variables, \code{harvest()} will
#' ignore those observations when raking on the variables where they are NA.
#' This effectively means that when raking an \code{age} variable, respondents
#' with missing age are assumed to be correctly proportioned by age. In
#' addition, calculates of weighted marginals (for instance, for error), ignore
#' NA respondents.
#'
#' An alternative strategy supported by \code{harvest()} is for the user to
#' specify \code{add_na_proportion}, an argument which will interpret missing
#' data as a "decline to state" response category, and also alter target
#' proportions to add such a category. The proportion missing in the
#' data is assumed to be the population "decline to state" proportion. Other
#' population proportions are adjusted accordingly, as if decline to state is
#' distributed randomly with respect to other such values. Documentation for
#' this argument is included above.
#'
#' In cases where systematic nonresponse to a question is a problem, users
#' might try external packages capable of imputing missing data, or else
#' alter target proportions and data to remove missingness.
#'
#' @section Specifying \code{target}:
#'
#' Target proportions contain three pieces of information: a variable-level pair
#' and an associated proportion. \code{target} can be specified one of two
#' ways: as a list of named vectors, or as a data frame.
#'
#' If a list of named vectors, the list names are variable names, the vector
#' names are variable levels, and the vector values are proportions. This is
#' the specification used in \code{anesrake}'s \code{anesrake} function, and
#' also the manner in which the built-in \code{\link{ns_target}} dataset is
#' specified.
#'
#' If a data frame, \code{harvest()} attempts to match variables in the data
#' frame to the three piece of information above. Matching occurs in the
#' following order:
#'
#' \enumerate{
#'   \item If the user provides a \code{target_map} argument, then
#'     \code{target_map} should be a named vector whose names are "variable",
#'     "level", and "proportion" and whose values are the numeric indices or
#'     column names of the respective data in the \code{target} argument.
#'   \item If columns named "variable", "level", and "proportion" exist in the
#'     \code{target} data frame, then these will be used.
#'   \item If the \code{target} data frame is exactly three columns, then the
#'     first column is assumed to contain "variable", the second to contain
#'     "level", and the third to contain "proportion". A warning will be
#'     generated if the user provided some but not all of \code{target_map}
#'   \item If none of these conditions hold, an error will be produced.
#' }
#'
#' @section Naming Weight Columns:
#'
#' If weights are attached to a data frame, the weights will be called
#' "weights" by default. If such a column already exists, the column will be
#' called ".weights.autumn". If this column already exists, ".weights_autumn1"
#' through ".weights_autumn10" will be used. If all of these columns exist,
#' \code{harvest()} will return an error. To customize the column name, use
#' the argument \code{weight_column} described above.
#'
#' @param data A data frame (tibble) or matrix containing data to be raked. The
#'   data can contain columns not used in the raking, but must contain all the
#'   columns used in the raking
#' @param target A list of target proportions in the population of interest.
#'   This argument can be one of two formats: a list of named numeric vectors,
#'   or a data frame (tibble). If a data frame, see the \code{target_map}
#'   argument and the "Specifying \code{target}" of this documentation for
#'   more details on the data frame's format. No level may have a negative
#'   proportion or an NA, and each variable should sum to 1.
#' @param start_weights Starting weights. This may either be a single positive
#'   number (which will be implicitly renormed to 1), or a vector of length n,
#'   where n is the number of rows in the data. No values in this vector may be
#'   NA, but some can be 0. Lovelace et al. (2015) found that initial weights
#'   generally have very little impact on final weight estimations. Selecting
#'   better initial weights may speed up convergence.
#' @param max_weight A maximum value to clamp weights to. By default, as per
#'   DeBell and Krosnick (2009) and \code{\link[anesrake]{anesrake}}, this is
#'   set to 5. Note: When weights exceed \code{max_weight}, all weights are
#'   truncated to \code{max_weight} and then re-distributed to have mean 1.
#'   This means capped weights may sometimes exceed \code{max_weight} in order
#'   to preserve weight mean = 1. To override this, see documentation for
#'   \code{enforce_mean}
#' @param max_iterations A maximum number of iterations per raking attempt.
#'   The default is 1,000. Note that the total number of iterations may exceed
#'   this number if after raking, additional variables display imbalance.
#'   Defaults in \code{\link[anesrake]{anesrake}} and \code{\link[mipfp]{Ipfp}}
#'   are 1,000. Default in \code{\link[survey]{rake}} is 10, but with
#'   considerably looser convergence critria.
#' @param convergence A named vector of convergence parameters. These are
#'   described below but the defaults are well-tuned for both speed and
#'   convergence to population marginals.
#' @param select_params A named vector of variable selection parameters. Which
#'   names to supply depends on the variable selection function. Parameters
#'   for built-in variable selection functions are described below.
#' @param select_function Specification of error function (how we measure which
#'   variables to rake on). This can either be a character vector specifying
#'   a built-in function, or a function closure (unquoted name of a function)
#'   which calculates a custom selection method. The built-in options are
#'   "threshold" (default), "all", "number", "lesser", or "greater". You can
#'   read more about these options below. Discussion of custom selection
#'   functions is also found below.
#' @param error_function Specification of error function (how we measure how
#'   far off a variable is from its intended result). This can be either a
#'   character vector specifying a built-in function, or a function closure (
#'   unquoted name of a function) which calculates a custom error rate. The
#'   built-in options are "linear" (default), "max", "squared", "mean",
#'   "maxsquared", and "meansquared". You can read more about these options
#'   below. "total", "average", "totalsquared", and "averagesquared" are also
#'   accepted for backwards compatibility with
#'   \code{\link[anesrake]{anesrake}}. Discussion of custom error functions
#'   is also found below.
#' @param verbose Level of verbosity, defaults to FALSE. At TRUE or 1, the
#'   function begins emitting progress information. At 2, each iteration
#'   provides a significant amount of progress information.
#' @param attach_weights A binary value, default TRUE. If FALSE, this function
#'   will return weights as a vector. If TRUE, this function will attach
#'   weights to the data frame provided and return the new data frame. The
#'   weights will be added in a column named "weights". If a column named
#'   "weights" is present, backup options will be used and the user will
#'   receive feedback.
#' @param weight_column A quoted character vector specifying a name for the
#'   column attached if \code{attach_weights} is TRUE. If a column with this
#'   name already exists, it will be overwritten.
#' @param add_na_proportion If TRUE, \code{harvest} will adjust the target
#'   proportions so that each variable has a proportion for missing data
#'   reflecting the missing data observed in the \code{data} sample. If a
#'   character vector, \code{harvest} will adjust the variables in the
#'   character vector but no others. If a numeric vector, \code{harvest} will
#'   adjust the variables based on num
#' @param target_map Used only if \code{target} is a data frame, provides a
#'   mapping from columns of the data frame to \code{variable}, \code{level},
#'   and \code{proportion}. Should be specified as a named vector, e.g.
#'   \code{target_map = c("variable" = 1, "level" = 2, "proportion" = 3)}. The
#'   values of this vector can be either numeric column indexes or quoted
#'   character vectors of column names.
#' @param enforce_mean Default TRUE. By default, weights minimize divergence
#'   from target proportions subject to two conditions: that the mean weight
#'   be 1, and that the maximum weight be capped at \code{max_weight}. Weights
#'   are first capped and then re-meaned. When \code{enforce_mean} is FALSE,
#'   the re-meaning does not occur. This will guarantee the maximum weight
#'   does not exceed \code{max_weight} but may result in mean weights diverging
#'   from 1. As \code{max_weight} prevents high-weight observations from
#'   becoming higher weighted, \code{enforce_mean} helps low-weight observations
#'   from becoming even lower weighted.
#' @param ... Additional arguments to this function are ignored
#' @return The original data frame \code{data} augmentd with a new column
#'   containing the calculated weights if \code{attach_weights} is TRUE
#'   (default). If \code{attach_weights} is FALSE, a vector of numeric
#'   weights in the order of the supplied cases.
#' @export
#' @examples
#' \dontrun{
#' # Simple call
#' harvest(respondent_data, ns_target)
#'
#' # Pipe workflow
#' respondent_data %>% harvest(ns_target)
#'
#' # Return weights as vector instead of attaching to data frame
#' harvest(respondent_data, ns_target, attach_weights = FALSE)
#'
#' # Modified convergence criteria to be more permissive
#' harvest(respondent_data, ns_target,
#'         convergence = c(
#'           pct = 0.05, absolute = 1e-4,
#'         ))
#'
#' # Limit runtime to 3 seconds:
#' harvest(respondent_data, ns_target,
#'         convergence = c(
#'           pct = 0.01, absolute = 1e-6,
#'           time = 3
#'         ))
#'
#' # Alternate error function or variable selection function:
#' harvest(respondent_data, ns_target,
#'         error_function = "meansquared",
#'         select_function = "number")
#'
#' # Generate an annoying amount of diagnostic information
#' harvest(respondent_data, ns_target, verbose = 2)
#' }
harvest = function(
  data,
  target,
  start_weights = 1,
  max_weight = 5,
  max_iterations = 1000,
  select_params = c(pct = 0.05, count = 5),
  convergence = c(pct = 0.01, absolute = 1e-6,
                  time = NULL, single_weight = NULL),
  select_function = "pct",
  error_function = "linear",
  verbose = FALSE,
  attach_weights = TRUE,
  weight_column = NULL,
  add_na_proportion = FALSE,
  target_map = NULL,
  enforce_mean = TRUE,
  ...
) {
  # Check if user supplied any weird arguments
  check_any_startup_issues(data, target, convergence, ...)

  # Set up initial weights centered at weight 1
  # sum / length is about twice as fast as mean.
  weights = generate_start_weights(data, start_weights, max_weight)
  weights = weights * length(weights) / sum(weights)

  # If the targets are a data frame instead of a list
  if(is.data.frame(target)) {
    target = df_targets_to_list(target)
  } else if(!is.list(target)) {
    stop("Target weights must be a list of named vectors or a data frame.")
  }

  # If user supplied a character for select/error function, map it. If not,
  # make sure the function behaves.
  select_function = get_select_function(select_function, select_params)
  error_function = get_error_function(error_function)

  # If user wants us to augment the target to replace NAs in a variable with
  # a fixed proportion from the sample.
  data_original = NULL
  if((is.logical(add_na_proportion) && add_na_proportion) ||
     (!is.logical(add_na_proportion) && length(add_na_proportion))) {
    if(verbose) {
      message("Adding weights for NA categories for one or more target ",
              "variables.")
    }

    # First replace the target proportions
    target = freeze_na_proportion(data, target, add_na_proportion)

    # Now replace the NAs in the data
    data_original = data
    data = update_na_values(data, target, add_na_proportion)
  }

  # Basic error checking in the data
  check_any_data_issues(data, target, weights)

  # Which variables are we going to rake on?
  which_rake = variable_selection(data, target, weights,
                                  select_function,
                                  select_params,
                                  error_function)
  variable_text = paste0(which_rake, collapse = ", ")
  if(verbose) {
    message("Beginning initial rake attempt with ",
            length(which_rake), " variables to rake on (",
            variable_text, ")")
  }

  # Rake once on the variables selected.
  base_time = Sys.time()
  weights = do_rake(data, target[which_rake], weights,
                    max_weight, max_iterations, convergence,
                    enforce_mean,
                    verbose)

  # Per DeBell and Krosnick, the past raking may have messed up variables which
  # were previously fine. We'll keep chasing that rabbit if need be.
  done = 0
  for(i in 1:9) {
    if("time" %in% names(convergence) &&
       !is.null(convergence[["time"]]) &&
       (difftime(Sys.time(), base_time, units = "secs") >
        convergence[["time"]])) {
      if(verbose) {
        message("Raking reached time limit specified in parameters.")
      }
      done = 2
      break
    }

    # Let's see if we want to rake any new variables we needn't before.
    # If there are no new variables, then we're as good as we're going to
    # get.
    new_rake = variable_selection(data, target, weights,
                                  select_function,
                                  select_params,
                                  error_function,
                                  error_none = FALSE)
    if(!any(!new_rake %in% which_rake)) {
      done = 1
      break
    }

    # If we have some new variables, let's add them to the rake list and go
    # again.
    if(verbose) {
      new_variable_text = paste0(setdiff(new_rake, which_rake),
                                 collapse = ", ")
      message("Outer loop ", (i + 1), ": Adding new vars to rake (",
              new_variable_text, ")")
    }
    which_rake = c(which_rake, setdiff(new_rake, which_rake))
    weights = do_rake(data, target[which_rake], weights,
                      max_weight, max_iterations, convergence,
                      enforce_mean, verbose)
  }

  if(!done) {
    warning("New variables became unbalanced even after 10 repetitions of ",
            "the entire raking process. Convergence may not be assured.")
  }

  if(verbose) {
    message("Process complete. Remaining post-weight errors on target ",
            "marginals: ")
    message(pretty_print_helper(
      get_current_miss(
        data,
        target,
        weights
      )))
  }

  # Return weights, unless user wants them attached to the input data frame,
  # which would be useful in a piped operation without using mutate.
  if(!is.null(weight_column) && !attach_weights) {
    message("Note: 'weight_column' specified even though ",
            "'attach_weights=FALSE'. Weights being returned as vector rather ",
            "than attached to data.")

  }
  if(!attach_weights) {
    return(weights)
  }

  if(!is.null(data_original)) {
    data = data_original
  }

  new_column_name = name_weight_column(data, weight_column)
  data[[new_column_name]] = weights
  attr(data, "target_symbol") = deparse(substitute(target))
  return(data)
}
