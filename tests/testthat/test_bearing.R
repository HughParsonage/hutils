context("Bearing")

test_that("Simple bearing", {
  expect_equal(bearing(-33, 151, -32, 151), 0)
})
