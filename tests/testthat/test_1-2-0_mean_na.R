context("mean_na")

test_that("mean na", {
  expect_equal(mean_na(1:5), 0)
  expect_equal(mean_na(NA), 1)
  expect_equal(mean_na(c(1:4, NA)), 0.2)
})

