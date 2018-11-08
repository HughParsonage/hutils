context("weight2rows 1.4.0")

test_that("rows.out < 1", {
  library(data.table)
  DT <- data.table(x = 1:10,
                   y = 10)
  expect_identical(weight2rows(DT, "y", rows.out = 0.1, discard_weight.var = TRUE),
                   DT[, .(x)])
})
