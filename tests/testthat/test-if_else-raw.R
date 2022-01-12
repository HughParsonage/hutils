test_that("if_else on raw", {
  x <- c(5L, 2L, 1L, 9L)
  y <- rev(x)
  expect_equal(if_else(1:4 > 2, as.raw(x), as.raw(y)),
               as.raw(if_else(1:4 > 2, x, y)))
})
