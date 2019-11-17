context("test-rake")

test_that("single variable rake works", {
  data = data.frame("var1" = c(rep("a", 3), "b"))
  targets = list("var1" = c("a" = 0.875, "b" = 0.125))
  weights = c(1, 1, 1, 1)
  select_function = select_pct
  select_params = c("pct" = 0.05)
  error_function = current_miss_linear

  expect_equal(
    expect_message(
      do_rake(
        data, targets, weights,
        max_weight = 5,
        max_iterations = 1000,
        convergence = c("pct" = 1e-6, "absolute" = 1e-6),
        verbose = 3),
      "is exactly correct"),
    c(1.16666666666667, 1.16666666666667, 1.16666666666667, 0.5)
  )

  # And verify clamping weights -- max weight should be exactly 5
  data2 = data.frame("var1" = c(rep("a", 100), "b"))
  targets2 = list("var1" = c("a" = 0.15, "b" = 0.85))
  weights2 = rep(1, 101)
  res = do_rake(data2, targets2, weights2, 5, 1000,
          convergence = c("pct" = 1e-6, "absolute" = 1e-6),
          enforce_mean = FALSE,
          verbose = 3)
  expect_equal(max(res), 5)

  # And verify ensure_mean
  res2 = do_rake(data2, targets2, weights2, 5, 1000,
                convergence = c("pct" = 1e-6, "absolute" = 1e-6),
                enforce_mean = TRUE,
                verbose = 3)
  expect_equal(mean(res2), 1)
})

test_that("testing fast convergence, timeout, regular convergence", {
  reduced_set = ns_target[1:5]
  expect_warning(
      harvest(respondent_data, reduced_set, verbose = 2),
      "Partial convergence")

  expect_message(
    harvest(respondent_data, ns_target,
            convergence = c(pct = 0.01, absolute = 1e-6, time = 0.01),
            verbose = 1),
    "time limit")
})
