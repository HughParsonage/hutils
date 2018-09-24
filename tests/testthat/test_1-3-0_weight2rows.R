context("weight2rows 1.3.0")

test_that("Error handling (1.3.0)", {
  library(data.table)
  expect_error(weight2rows(1:5), 
               "must be a data.frame")
  
  dt <- data.table(x = 1:10,
                   y = c(9.73, 9.64, 8.82, 2.42, 0.76, 2.93, 9.18, 4.77, 5.93, 8))
  expect_error(weight2rows(dt), 
               regexp = " is missing")
  expect_error(weight2rows(dt, 1:2), 
               "`weight.var` had length 2.",
               fixed = TRUE)
  expect_error(weight2rows(dt, 3), 
               "outside the range")
  expect_error(weight2rows(dt, raw(1)),
               "`typeof(weight.var) = 'raw'", 
               fixed = TRUE)
  expect_error(weight2rows(dt, "z"), 
               regexp = "not a column name of DT")
})
  
test_that("Error handling (rows.out)", {  
  library(data.table)
  dt <- data.table(x = 1:10,
                   y = c(9.73, 9.64, 8.82, 2.42, 0.76, 2.93, 9.18, 4.77, 5.93, 8))
  expect_error(weight2rows(dt, "y", rows.out = raw(1)), 
               regexp = "typeof(rows.out) = 'raw'",
               fixed = TRUE)
  expect_error(weight2rows(dt, "y", rows.out = numeric(2)), 
               regexp = "length(rows.out) = 2",
               fixed = TRUE)
  expect_error(weight2rows(dt, "y", rows.out = NA_real_), 
               regexp = "`rows.out = NA` but NA is not permitted.",
               fixed = TRUE)
})

test_that("Preserves colorder", {
  library(data.table)
  DT <- data.table(x = 1:5, y = c(1, 1, 1, 1, 2))
  expect_equal(weight2rows(DT, "y"),
               data.table(x = c(1:5, 5L), y = 1L))
})

test_that("Doesn't update original if not a data.table", {
  library(data.table)
  DT <- data.table(x = 1:5, y = c(1, 1, 1, 1, 2))
  DF <- data.frame(x = 1:5, y = c(1, 1, 1, 1, 2))
  
  # Don't mandate that output is a data.table.
  expect_identical(as.data.table(weight2rows(DF, "y")),
                   as.data.table(weight2rows(DT, "y")))
  expect_false(is.data.table(DF))
})

test_that("rows.out", {
  library(data.table)
  dt <- data.table(x = 1:10,
                   y = c(9.73, 9.64, 8.82, 2.42, 0.76, 2.93, 9.18, 4.77, 5.93, 8))
  res <- weight2rows(dt, 2L, rows.out = 20)
  expect_true(between(nrow(res), 18L, 22L))
})

test_that("Discarding weight.var", {
  library(data.table)
  dt <- data.table(x = 1:10,
                   y = c(9.73, 9.64, 8.82, 2.42, 0.76, 2.93, 9.18, 4.77, 5.93, 8))
  res <- weight2rows(dt, 2L, rows.out = 20, discard_weight.var = TRUE)
  expect_true(between(nrow(res), 18L, 22L))
  expect_equal(ncol(res), 1)
})

