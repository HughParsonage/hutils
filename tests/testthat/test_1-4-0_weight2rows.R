context("weight2rows 1.4.0")

test_that("rows.out < 1", {
  library(data.table)
  DT <- data.table(x = 1:10,
                   y = 10)
  expect_identical(weight2rows(DT, "y", rows.out = 0.1, discard_weight.var = TRUE),
                   DT[, .(x)])
})

test_that("weight includes 0", {
  library(data.table)
  DT <- data.table(x = 1:2,
                   y = c(10L, 0L))
  expect_equal(nrow(weight2rows(DT, "y")), 10L)
})

