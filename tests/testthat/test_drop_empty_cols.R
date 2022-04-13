test_that("test_drop_empty_cols", {
  skip_if_not_installed("data.table")
  library(data.table)
  DT <- data.table(x1 = rep(NA, 10), 
                   x2 = c(NA, NA, logical(8)),
                   x3 = c(NA, NA, rep(TRUE, 8)),
                   y = 1:10)
  expect_equal(names(drop_empty_cols(DT)), c("x2", "x3", "y"))
})