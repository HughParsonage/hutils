context("Drop cols")

test_that("Drop column", {
  library(data.table)
  DT <- data.table(x = 1, y = 1)
  DT_out <- drop_col(DT, "x")
  expect_equal(names(DT_out), "y")
  
  DT_out2 <- drop_col(DT, "x", checkDT = FALSE)
  expect_equal(names(DT_out2), "y")
  
  expect_error(drop_col(as.data.frame(DT), "x"))
  expect_error(drop_col(DT, c("x", "y")))
  
  DT_multiple <- data.table(x = 1:2, y = 1:2, z = 1:2)
  setnames(DT_multiple, 2, "x")
  expect_error(drop_col(DT_multiple, "x"))
})

test_that("Drop columns", {
  library(data.table)
  DT <- data.table(x = 1, y = 2, z = 3)
  
  DT_out <- drop_cols(DT, c("x", "y"))
  expect_equal(names(DT_out), "z")
  
  DT_out2 <- drop_cols(DT, c("x", "y"), checkDT = FALSE)
  expect_equal(names(DT_out2), "z")
  
  DT_multiple <- data.table(x = 1:2, y = 1:2, z = 1:2)
  setnames(DT_multiple, 2, "x")
  expect_error(drop_col(DT_multiple, "x"),
               regexp = "DT had 2 columns named 'x', but drop_col() only accepts singular columns. Either use drop_cols() to drop multiple columns or ensure `DT` has distinct column names.", 
               fixed = TRUE)
})

test_that("Drop colr", {
  library(data.table)
  DT <- data.table(x = 1, y = 2, z = 3)
  DT_out <- drop_cols(DT, "x")
  expect_equal(names(DT_out), c("y", "z"))
})
