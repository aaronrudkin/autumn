#' Adjust weights based on current imbalance of a single variable.
#'
#' This function internally rakes the weights provided according to the
#' current imbalance in a single variable. The mathematical process here is
#' simple: for each level of the variable, multiply the weight by the target
#' proportion divided by the current weighted proportion. For instance, if a
#' \code{party} variable should be 0.4 Independent, 0.35 Democratic, and 0.25
#' Republican, and the current weighted proportion is 0.4 Democratic, then the
#' weights of every Democrat will be multiplied by 0.35 / 0.4. This is an
#' internal function and not intended for end-users.
#'
#' @param data The data frame (tibble) or matrix which contains the original
#'   data.
#' @param weights The current weights
#' @param target The target proportions, specified as in \code{\link{harvest}}
#' @param var A quoted character vector containing the variable we are currently
#'   raking on.
#' @param cache A list of integer vectors mapping rows of the data frame to
#'   levels of the target variable
#' @return Adjusted weights, raked for this variable.
#' @keywords internal
single_adjust = function(data, weights, target, var, cache) {
  # First, get the current weight balance in the population
  current = weighted_pct(data[[var]], weights)[names(target[[var]])]

  # If you've got an NA for this variable, you don't get multiplied on it.
  # If not, you get multiplied by the ratio between the target and the current
  # Why do this as two steps rather than if-else? Twice as fast
  mult = unname(target[[var]] / current)[cache[[var]]]
  mult[is.na(data[[var]])] = 1

  # Return the new weights, not just the multipliers
  return(weights * mult)
}

#' Clamp weights to a maximum weight
#'
#' @param weights Current vector of weights
#' @param clamp A scalar maximum weight
#' @return A vector of clamped weights
#' @keywords internal
clamp_weights_top = function(weights, clamp) {
  # Parallel minimum: if a weight is above the maximum, clamp it down.
  # pmin is faster than other options; copying the vector and overwriting
  # using a subset is somewhat faster but allocates much more memory.
  pmin(weights, clamp)
}

#' Performs iterative raking on data
#'
#' This function performs iterative raking as described in DeBell and Krosnick
#' (2009). It is a private function and not intended for end-users. End-users
#' should call \code{\link{harvest}} to add weights.
#'
#' @param data The data frame (tibble) or matrix containing the original data
#' @param target The target proportions, specified as in \code{\link{harvest}}
#' @param weights A numeric vector of current weights
#' @param max_weight The maximum weight to clamp weights to after raking each
#'   variable
#' @param max_iterations The maximum number of iterations of raking to perform
#'   before giving up. Please note that depending on the variable selection
#'   method, \code{\link{harvest}} may continue iterative raking on new
#'   variables after completing the iterations of this function call.
#' @param convergence Convergence parameters as described in
#'   \code{\link{harvest}}.
#' @param verbose Level of verbosity, defaults to FALSE. At TRUE or 1, the
#'   function begins emitting progress information during major events. At 2,
#'   each iteration provides progress information.
#' @inheritParams harvest
#' @return A numeric vector of adjusted, raked weights.
#' @keywords internal
do_rake = function(data, target, weights,
                   max_weight, max_iterations, convergence,
                   enforce_mean = TRUE,
                   verbose) {
  # Get current time
  base_time = Sys.time()

  # Pocket algorithm: we want to start this with a very high value
  weight_update_sum = 1e9

  # In the actual raking, we're going to get multipliers for each level of
  # a given target variable. So, for instance, we decide that "LLCs" get a
  # multiplier of 2, and "S-Corps" get a multiplier of 0.4 and ... -- now we
  # need to map those multipliers to rows. We do this by seeing which rows are
  # each thing by sgtring matching. But for a few reasons, doing this lookup
  # is only fast if the columns of the data frame are all factors. We can
  # avoid paying this cost by pre-caching it across all iterations.
  pre_cache = lapply(names(target), function(variable) {
    match(data[[variable]], names(target[[variable]]))
  })
  names(pre_cache) = names(target)

  # We're going to rake for a maximum number of iterations
  for(i in 1:max_iterations) {
    # Let the user know we're starting the iteration, and if it's not the first,
    # how much the weights changed since last time.
    if(verbose > 1) {
      message("Beginning iteration ", i,
              ifelse(i > 1,
                     paste0(" (total weight update: ", weight_update_sum, ")"),
                     ""))
    }

    # We need this to benchmark how much the weights change
    old_weights = weights

    # Rake each variable one at a time.
    for(j in names(target)) {
      if(verbose > 2) message("  Raking variable: ", j)
      weights = single_adjust(data, weights, target, j, pre_cache)
    }

    # Clamp weights if necessary -- why sum / length? Faster than mean.
    clamp_offset = 1e-4
    if(max(weights) > max_weight + clamp_offset) {
      if(verbose > 1) message("  Clamping weights.")
      weights = clamp_weights_top(weights, max_weight)
      if(enforce_mean) weights = weights / (sum(weights) / length(weights))
    }

    # If there's only one variable in the rake set, it is by definition raked
    # after a single iteration
    if(length(target) == 1) {
      if(verbose > 1) message("  Single variable rake is exactly correct.")
      weight_update_sum = 0
      break
    }

    # Assess whether the weights are changing or stable.
    weight_update_old = weight_update_sum
    weight_update_sum = sum(abs(weights - old_weights))

    # The amount the weights updated are too close to the amount the weights
    # updated last time, so we've converged.
    if(weight_update_sum > weight_update_old * (1 - convergence[["pct"]])) {
      if(verbose > 1) {
        message("Convergence at iteration ", i, " based on weight update > ",
                (100 * (1 - convergence["pct"])), "% of previous iteration.")
      }
      break
    }

    # The weights have barely updated, let's finish faster
    if(weight_update_sum < convergence[["absolute"]]) {
      if(verbose > 1) {
        message("Convergence based on total weight update < ",
                sprintf("%.8f", convergence[["absolute"]]))
      }
      break
    }

    # The weights have barely updated, let's finish faster.
    if("single_weight" %in% names(convergence) &&
       max(abs(weights - old_weights)) < convergence[["single_weight"]]) {
      if(verbose > 1) {
        message("Convergence based on max weight update < ",
                sprintf("%.8f", convergence[["single_weight"]]))
      }
      break
    }

    # If user specified a timeout, timeout the raking process.
    if("time" %in% names(convergence) &&
       !is.null(convergence[["time"]]) &&
       (difftime(Sys.time(), base_time, units = "secs") >
        convergence[["time"]])) {
      break
    }
  }

  # Note to the user if the weights were still in a state of flux when
  # we stopped processing -- versus ANESrake, this warning is based on the
  # overall length of the weights, since something with more observations
  # can have more updating with relatively speaking substantive impact.
  if(weight_update_sum > 0.001 * length(weights)) {
    warning("Partial convergence only after ", i, " iterations: ",
            weight_update_sum, " / ", weight_update_old)
  }

  # Return weights
  weights
}
