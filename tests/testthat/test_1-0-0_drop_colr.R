context("drop_colr")

test_that("Simple error handling", {
  DF <- data.frame(x = 1, y = 2)
  expect_error(drop_colr(DF, "x"))
})


