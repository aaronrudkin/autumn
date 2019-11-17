context("test-augment_nas")

test_that("augmenting proportions with NAs", {
  result = freeze_na_proportion(respondent_data, ns_target, TRUE)

  # Ensure an additional proportion has been added to this variable
  expect_equal(length(result[["household_income"]]), 10)

  # But it still sums to 1
  expect_equal(sum(result[["household_income"]]), 1)

  # Proportions should make sense: Should be ~9.7% missing
  prop_nas = nrow(respondent_data[is.na(respondent_data$household_income), ]) /
    nrow(respondent_data)
  expect_equal(
    result[["household_income"]][["___NA"]],
    prop_nas
  )

  # Also check the specification by numeric and character
  expect_equal(
    freeze_na_proportion(respondent_data, ns_target, 5),
    result
  )
  expect_equal(
    freeze_na_proportion(respondent_data, ns_target, "household_income"),
    result
  )

  # And that if you just override a variable with no NA, you don't get
  # additional levels
  expect_equal(
    length(freeze_na_proportion(respondent_data, ns_target, "race")[["race"]]),
    4
  )
})

test_that("correctly re-label NAs in the actual data", {
  result = update_na_values(respondent_data, ns_target, TRUE)
  # Ensure we no longer have any NAs
  expect_equal(
    sum(is.na(result$household_income)),
    0
  )

  # And the ___NAs we have match the number of NAs before
  expect_equal(
    sum(result$household_income == "___NA"),
    nrow(respondent_data[is.na(respondent_data$household_income), ])
  )

  # It also works for the numeric and named versions
  expect_equal(
    update_na_values(respondent_data, ns_target, 5),
    result
  )

  expect_equal(
    update_na_values(respondent_data, ns_target, "household_income"),
    result
  )

  # And if we don't specify a variable, it doesn't update.
  r2 = update_na_values(respondent_data, ns_target, "race")
  expect_equal(
    sum(is.na(r2$household_income)),
    nrow(respondent_data[is.na(respondent_data$household_income), ])
  )
})
