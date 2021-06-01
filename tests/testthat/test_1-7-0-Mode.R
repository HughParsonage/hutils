test_that("Sparse ints", {
  x <- c(-.Machine$integer.max, 0L, 0L, .Machine$integer.max)
  expect_equal(Mode(x), 0L)
  x <- c(-.Machine$integer.max, 0L, 0L, NA, .Machine$integer.max)
  expect_equal(Mode(x), 0L)
})
