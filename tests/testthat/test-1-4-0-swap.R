context("test-1-4-0-swap")

test_that("swap works", {
  a <- 1
  b <- 2
  a %<->% b
  expect_equal(a, 2)
  expect_equal(b, 1)
})

test_that("swap errors", {
  "***temp***" <- NULL
  a <- list()
  b <- c()
  expect_error(a %<->% b)  # don't worry about error message for now
})
