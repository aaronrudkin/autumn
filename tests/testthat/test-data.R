context("test-data")

test_that("data is coherent", {
  expect_equal(dim(respondent_data), c(6703, 18))
  expect_length(ns_target, 17)
  expect_known_hash(names(ns_target), "23d42b4c87")
})
