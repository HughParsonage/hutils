context("test-1-4-0-swap")

test_that("swap works", {
  a <- 1
  b <- 2
  a %<->% b
  expect_equal(a, 2)
  expect_equal(b, 1)
})
