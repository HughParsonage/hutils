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


test_that("Logical weight still works", {
  # Just for coverage
  library(data.table)
  DT <- data.table(x = 1:10,
                   ww = as.logical(1:10 %% 3L))
  DT4 <- suppressWarnings(weight2rows(DT, "ww", rows.out = 4L))
  expect_equal(nrow(DT4), 4L)
  DT4_no_w <- suppressWarnings(weight2rows(DT, "ww", rows.out = 4L, discard_weight.var = TRUE))
  expect_false("ww" %in% names(DT4_no_w))
})


