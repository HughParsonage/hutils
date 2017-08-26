context("drop_colr")

test_that("Simple error handling", {
  DF <- data.frame(x = 1, y = 2)
  expect_error(drop_colr(DF, "x"))
})

test_that("Outcomes", {
  library(data.table)
  DT <- data.table(abcde_ghi = 1, STE_2016 = 2)
  DT_out <- drop_colr(DT, "[A-Z]{3}")
  expect_equal(names(DT_out), "abcde_ghi")
})
