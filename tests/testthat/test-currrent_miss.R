context("test-current_miss")

test_that("current miss of data", {
  result = get_current_miss(respondent_data, ns_target)

  expect_length(result, length(ns_target))
  expect_named(result, names(ns_target))
  expect_known_hash(result, "79f67a3112")

  # Making sure the code that decodes a character vector to a closure works
  result2 = get_current_miss(respondent_data, ns_target,
                             error_function = "linear")

  expect_equal(result, result2)
})

test_that("error functions", {
  truth = c(1, 2, 3)
  current = c(0, 1, 10)
  expect_equal(current_miss_linear(truth, current), 9)
  expect_equal(current_miss_squared(truth, current), 51)
  expect_equal(current_miss_mean(truth, current), 3)
  expect_equal(current_miss_max(truth, current), 7)
  expect_equal(current_miss_max_squared(truth, current), 49)
  expect_equal(current_miss_mean_squared(truth, current), 17)
})
