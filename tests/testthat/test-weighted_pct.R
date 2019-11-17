context("test-weighted_pct")

test_that("unweighted case", {
  x = c(rep("x", 5), rep("y", 15))
  weights = rep(1, 20)
  result = weighted_pct(x, weights)
  expect_true(all.equal(unname(result), c(0.25, 0.75)))
  expect_equal(names(result), c("x", "y"))
})

test_that("weighted case", {
  x = c(rep("x", 5), rep("y", 15))
  weights = c(rep(3, 5), rep(1, 15))
  result = weighted_pct(x, weights)
  expect_true(all.equal(unname(result), c(0.5, 0.5)))
  expect_equal(names(result), c("x", "y"))
})
