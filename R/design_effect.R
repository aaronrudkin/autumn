#' Calculate design effect from weights
#'
#' This function calculates the design effect of the supplied weights. A design
#' effect is an estimate of the variance inflation caused by the design of a
#' study. If a vector of weights is provided without any outcome, this function
#' implements the estimator in Kish (1992), which assumes no correlation between
#' provided weights and an outcome of interest. If an outcome of interest is
#' provided, this function implements the estimator in Spencer (2000), which
#' accounts for correlation between outcomes and weights. For a broader
#' discussion of design effects in weighted surveys, see Spencer (2000).
#'
#' @param weights The weights for which a design effect is desired
#' @param outcome A vector outcome of interest.
#' @return A numeric design effect for the weights
#' @importFrom stats coef cor lm resid var
#' @export
design_effect = function(weights, outcome = NULL) {
  if(is.data.frame(weights)) {
    cand_weights = c("weights", paste0(".weights_autumn", 1:10))
    weight_var = cand_weights[which(cand_weights %in% colnames(weights))[1]]
    if(is.na(weight_var)) {
      stop("Error: design_effect requires a `weights` argument. If the ",
           "argument specified is a data frame, it must contain a column ",
           "named weights. Please specify weights directly or correct data ",
           "frame.")
    }

    weights = weights[[weight_var]]
  }

  # Kish (1992) estimator if no outcome is provided
  kish_estimate = length(weights) * sum(weights^2) / sum(weights)^2
  if(is.null(outcome)) return(kish_estimate)

  # Basic error checking
  if(length(outcome) != length(weights)) {
    stop("Error: The provided outcome is not equal in length to the provided ",
         "weights.")
  }

  if(!is.numeric(outcome)) {
    stop("Error: The provided outcome must be numeric.")
  }

  # TODO: EVERYTHING BELOW IS NOT YET CHECKED
  # TODO: EVERYTHING BELOW IS NOT YET TESTED
  #nolint start
  # Now, calculate what we need for the Spencer (2000) outcome.
  # (1 - rho^2)(1 + rvw) + (alpha / sigma_y^2)^2 (rvw)
  # kish_estimate = 1 + rvw
  #nolint end

  # I don't think this is the right probability of selection
  prob_selection = 1 / (length(weights) * weights)
  model = lm(outcome ~ prob_selection, weights = weights)

  weighted_mean_y = sum(weights * outcome) / sum(weights)
  weighted_pop_var = sum(weights * (outcome - weighted_mean_y)^2) /
    sum(weights)

  resid_var = var(resid(model))
  cor_outcome_prob = cor(outcome, prob_selection)
  est_intercept = unname(coef(model)[1])

  ((1 - cor_outcome_prob^2) * kish_estimate) +
    ((kish_estimate - 1) * (est_intercept / sqrt(weighted_pop_var))^2)
}

#' Calculate effective sample size from weights
#'
#' This function calculates the effective sample size implied by the
#' supplied weights. Because weighting inflates variance of quantities of
#' interest, the effective sample size of a weighted sample is less than the
#' nominal sample size. This function calls \code{\link{design_effect}}. If an
#' outcome is not provided, \code{\link{design_effect}} implements the
#' estimator from Kish (1992), which assumes no correlation between weights
#' and outcomes. If an outcome  is provided, the function implements Spencer
#' (2000).
#'
#' @param weights The weights for which an implied effective sample size is
#'   designed
#' @param outcome An vector outcome of interest. If NULL, this function
#'   returns the Kish (1992) estimator of design effects. If numeric data is
#'   provided, this function returns the Spencer (2000) estimator of design
#'   effects conditional on the correlation between supplied weights and the
#'   outcome of interest.
#' @return A numeric effective sample size, which may be non-integer.
#' @export
effective_sample_size = function(weights, outcome = NULL) {
  if(is.data.frame(weights)) {
    return(nrow(weights) / design_effect(weights, outcome))
  }

  return(length(weights) / design_effect(weights, outcome))
}
