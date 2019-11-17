context("test-harvest")

test_that("unusual argument combinations for harvest", {
  skip_on_cran()

  df_target = list_targets_to_df(ns_target)

  # Invalid format targets
  expect_error(harvest(respondent_data, 3))

  # Asked for weight_column but not attach_weights
  expect_message(
    harvest(respondent_data, df_target,
            add_na_proportion = TRUE,
            verbose = 1,
            max_iterations = 5,
            attach_weights = FALSE,
            weight_column = "new_w"),
    "Weights being returned as vector")

  # NA imputation
  expect_message(
    harvest(respondent_data, df_target,
            add_na_proportion = TRUE,
            verbose = 1,
            max_iterations = 5),
    "Adding weights for NA")

  # Actually add the weight
  base_dim = dim(respondent_data)
  expect_equal(
    dim(harvest(respondent_data, ns_target,
                   add_na_proportion = TRUE,
                   attach_weights = TRUE))[2],
    dim(respondent_data)[2] + 1)
})
