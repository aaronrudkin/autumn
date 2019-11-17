context("test-variable_selection")

test_that("individual select functions", {
  # d > e so that we can test the correct ordering for number
  misses = c("a" = 0.05, "b" = 0.07, "c" = 0.08, "d" = 0.15, "e" = 0.11)

  pct = select_pct(misses, c("pct" = 0.079))
  number = select_number(misses, c("count" = 4))
  all_results = select_all(misses, c("whatever"))
  lesser = select_lesser(misses, c("pct" = 0.079, "count" = 4))
  lesser_alt = select_lesser(misses, c("pct" = 0.079, "count" = 2))
  greater = select_greater(misses, c("pct" = 0.079, "count" = 4))
  greater_alt = select_greater(misses, c("pct" = 0.079, "count" = 2))

  expect_length(pct, 3)
  expect_length(number, 4)
  expect_length(all_results, 5)
  expect_length(lesser, 3)
  expect_length(greater, 4)
  expect_length(lesser_alt, 2)
  expect_length(greater_alt, 3)

  expect_equal(names(pct), c("c", "d", "e"))
  expect_equal(names(number), c("b", "c", "d", "e"))
  expect_equal(pct, lesser)
  expect_equal(number, greater)
  expect_equal(all_results, misses)

  expect_known_hash(pct, "711bf2c5d0")
  expect_known_hash(number, "40111d220b")
  expect_known_hash(all_results, "d94b23f4ee")
})

test_that("error when variable selection called with no unweighted", {
  data = data.frame("var1" = c(rep("a", 3), "b"))
  targets = list("var1" = c("a" = 0.75, "b" = 0.25))
  weights = c(1, 1, 1, 1)
  select_function = select_pct
  select_params = c("pct" = 0.05)
  error_function = current_miss_linear
  expect_error(
    variable_selection(
      data, targets, weights,
      select_function, select_params,
      error_function))
})
