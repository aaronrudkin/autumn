context("test-anesrake-passthrough")

test_that("anesrake passthrough", {
  skip_on_cran()

  result1 = harvest(respondent_data, ns_target, attach_weights = FALSE)

  # Capture the anesrake passthrough -- and the two conditionals updating
  # arguments on the fly, and the message about providing a caseid or one
  # of the other deprecated arguments.
  result2 = anesrake(ns_target, respondent_data)
  result3 = anesrake(ns_target, respondent_data, respondent_data$ResponseID,
                     pctlim = 0.05, weightvec = rep(1, nrow(respondent_data)),
                     filter = rep(1, nrow(respondent_data)),
                     iterate = TRUE)

  # To capture the warning temporarily, we have to use arrow assignment to
  # disambiguate the assignment from the function arguments.
  expect_warning(result4 <- anesrake(
    ns_target, respondent_data, caseid = rep(1, 25)))

  expect_equal(result1, result2$weightvec)
  expect_equal(names(result3$weightvec), respondent_data$ResponseID)
  expect_equal(result2$weightvec, unname(result3$weightvec))
  expect_equal(result2$weightvec, result4$weightvec)
  expect_error(anesrake(ns_target, respondent_data, filter = rep(1, 20)))


})
