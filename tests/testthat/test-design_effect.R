context("test-design_effect")

test_that("basic code runthroughs of Kish effect and ESS", {
  weights = rep(1, 100)
  expect_equal(design_effect(weights), 1)
  expect_equal(effective_sample_size(weights), 100)

  df_w = data.frame(id = 1:100, weights = rep(1, 100))
  expect_equal(effective_sample_size(df_w), 100)

  df_w2 = data.frame(id = 1:100, w2 = rep(1, 100))
  expect_error(design_effect(df_w2), "Please specify weights directly")

  expect_error(
    design_effect(weights, outcome = rep(1, 50)),
    "not equal in length"
  )

  expect_error(
    design_effect(weights, outcome = rep("a", 100)),
    "must be numeric"
  )
})
