context("test-setup")

test_that("error conditions in check startup issues", {
  # Convergence is NULL
  expect_error(
    check_any_startup_issues(respondent_data, ns_target, convergence = NULL)
  )

  # Missing one of pct or absolute
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = 0.01))
  )
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = 1e-6))
  )

  # Negative time
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = 1e-6, absolute = 1e-6,
                                           time = -1))
  )

  # Negativ single_weight
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = 1e-6, absolute = 1e-6,
                                             single_weight = -1))
  )

  # Invalid values
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = -1, absolute = 1e-6))
  )
  expect_error(
    check_any_startup_issues(respondent_data, ns_target,
                             convergence = c(pct = 0.01, absolute = 2))
  )

  # Data isn't a valid type
  expect_error(check_any_startup_issues(
    data = 1:10, ns_target, convergence = c(pct = 0.01, absolute = 1e-6)))

  # Unknown extra argumnts
  expect_message(check_any_startup_issues(
    respondent_data, ns_target, convergence = c(pct = 0.01, absolute = 1e-6),
    test_extra = 1))
})

test_that("error conditions in check data issues", {
  weights = rep(1, nrow(respondent_data))
  # Variable in target not in data
  d2 = respondent_data[, -1]
  expect_error(check_any_data_issues(d2, ns_target, weights))

  # Level in target not in data
  d3 = respondent_data[respondent_data$gender != "Male", ]
  expect_error(check_any_data_issues(d3, ns_target, weights))

  # Level in data not in target
  d4 = respondent_data
  d4$language[1] = "French"
  expect_error(check_any_data_issues(d4, ns_target, weights))

  # Proportion doesn't sum to 1
  t2 = ns_target
  t2[["gender"]][1] = 1.2
  expect_error(check_any_data_issues(respondent_data, t2, weights))

  # Proportion sums to 1 but includes negative
  t2[["gender"]] = c("Male" = -0.2, "Female" = 1.2)
  expect_error(check_any_data_issues(respondent_data, t2, weights))

  # Negative weights
  w2 = weights
  w2[1] = -1
  expect_error(check_any_data_issues(respondent_data, ns_target, w2))

  # Weights not equal in length
  w2 = c(0.2, 0.4)
  expect_error(check_any_data_issues(respondent_data, ns_target, w2))

  # Works fine otherwise
  expect_null(check_any_data_issues(respondent_data, ns_target, weights))
})

test_that("generating start weights", {
  max_weight = 5
  weights = rep(1, nrow(respondent_data))
  expect_length(generate_start_weights(respondent_data, 1, max_weight),
                nrow(respondent_data))

  expect_error(generate_start_weights(respondent_data, c(1, 2), max_weight))
  weights[1] = NA
  expect_error(generate_start_weights(respondent_data, weights, max_weight))
  weights[1] = max_weight + 1
  expect_warning(generate_start_weights(respondent_data, weights, max_weight))

  weights = sample(1:5, nrow(respondent_data), replace = TRUE)
  expect_equal(generate_start_weights(respondent_data, weights, max_weight),
               weights)
})

test_that("conversion of tibble/df targets to vector", {
  test_target = data.frame(
    "var" = c("x", "x", "x", "y", "y", "y", "z", "z"),
    "lvl" = c("a", "b", "c", "e", "d", "f", "g", "h"),
    "prop" = c(0.5, 0.4, 0.1, 0.4, 0.4, 0.2, 0.7, 0.3)
  )

  results = df_targets_to_list(test_target)

  expect_length(results, 3)
  expect_length(results[[1]], 3)
  expect_named(results, c("x", "y", "z"))
  expect_named(results[[2]], c("e", "d", "f"))

  # Did it generate correctly
  expect_known_hash(results, "4600022090")

  expect_warning(
    df_targets_to_list(test_target, target_map = c("variable" = 1))
  )

  # Manually specify columns still OK
  test_target = test_target[, c(1, 3, 2)]
  expect_known_hash(
    df_targets_to_list(
      test_target,
      target_map =
        c("variable" = 1,
          "proportion" = 2,
          "level" = 3)),
    "4600022090")

  # Works as a character string too
  expect_known_hash(
    df_targets_to_list(
      test_target,
      target_map =
        c("variable" = "var",
          "proportion" = "prop",
          "level" = "lvl")),
    "4600022090")

  # Too many columns to not specify
  test_target$extra = 1:8
  expect_error(df_targets_to_list(test_target))

  # Infer from column names
  colnames(test_target) = c("variable", "proportion", "level", "extra")
  expect_known_hash(
    df_targets_to_list(test_target),
    "4600022090")

})

test_that("error function switch", {
  #nolint start
  bad_func_big = function(x, y, z) { }
  bad_func_little = function(x) { }
  good_func_little = function(...) { }
  #nolint end

  # Check all valid inputs
  expect_equal(autumn:::get_error_function("total"),
               autumn:::current_miss_linear)
  expect_equal(autumn:::get_error_function("max"),
               autumn:::current_miss_max)
  expect_equal(autumn:::get_error_function("squared"),
               autumn:::current_miss_squared)
  expect_equal(autumn:::get_error_function("linear"),
               autumn:::current_miss_linear)
  expect_equal(autumn:::get_error_function("mean"),
               autumn:::current_miss_mean)
  expect_equal(autumn:::get_error_function("average"),
               autumn:::current_miss_mean)
  expect_equal(autumn:::get_error_function("totalsquared"),
               autumn:::current_miss_squared)
  expect_equal(autumn:::get_error_function("maxsquared"),
               autumn:::current_miss_max_squared)
  expect_equal(autumn:::get_error_function("meansquared"),
               autumn:::current_miss_mean_squared)
  expect_equal(autumn:::get_error_function("averagesquared"),
               autumn:::current_miss_mean_squared)

  # Supplied character but not standard
  expect_warning(get_error_function("invalid error function"))

  # Not character or closure
  expect_error(get_error_function(4))

  # Too many arguments
  expect_warning(get_error_function(bad_func_big))

  # Too few arguments
  expect_error(get_error_function(bad_func_little))

  # Too few arguments but it's a ...
  expect_equal(get_error_function(good_func_little),
               good_func_little)
})

test_that("generating names for weight columns", {
  d = data.frame(a = 1)
  expect_equal(name_weight_column(d, "otest"),
               "otest")
  expect_equal(name_weight_column(d), "weights")
  d$weights = 2
  expect_message(name_weight_column(d))
  expect_equal(name_weight_column(d), ".weights_autumn")
  d$.weights_autumn = 3
  expect_equal(name_weight_column(d), ".weights_autumn1")
  d$.weights_autumn1 = 4
  expect_equal(name_weight_column(d), ".weights_autumn2")
  for(i in 2:10) {
    d[[paste0(".weights_autumn", i)]] = 5
  }
  expect_error(name_weight_column(d))
})

test_that("param check for selection function", {
  # Missing a parameter that's required
  expect_error(
    check_select_params("pct", c("test" = 1))
  )

  expect_silent(
    check_select_params("pct", c("pct" = 0.03))
  )

  expect_silent(
    check_select_params("unknown function", c("params" = 1))
  )
})

test_that("get select functions", {
  # Picked a valid one
  expect_equal(
    get_select_function("pct", c("pct" = 0.03)),
    select_pct
  )

  # Picked an valid one but had invalid params
  expect_error(
    get_select_function("pct", c("total" = 0.03))
  )

  # Picked an invalid function, got a warning, defaulted to select_pct
  expect_equal(
    expect_warning(get_select_function("invalid", c("pct" = 0.03))),
    select_pct
  )

  # Supplied an argument of totally unknown type
  expect_error(
    get_select_function(1, c("pct" = 0.05))
  )

  #nolint start
  bad_func_big = function(x, y, z) { }
  bad_func_small = function(x) { }
  good_func_small = function(...) { }
  #nolint end

  # Supplied a function with too many arguments
  expect_warning(
    get_select_function(bad_func_big, c("param" = 1))
  )

  # Supplied a function with too few arguments
  expect_error(
    get_select_function(bad_func_small, c("param" = 1))
  )

  # Supplied a ... function:
  expect_equal(
    get_select_function(good_func_small, c("param" = 1)),
    good_func_small
  )

})
