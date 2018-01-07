context("Set cols first")

test_that("Cols first and last", {
  DT <- data.table(y = 1:5, z = 11:15, x = letters[1:5])
  set_cols_first(DT, "x")
  
  expect_equal(names(DT), c("x", "y", "z"))
  
  set_cols_first(DT, "z")
  expect_equal(names(DT), c("z", "x", "y"))
  
  set_cols_first(DT, "y")
  expect_equal(names(DT), c("y", "z", "x"))
  set_cols_last(DT, "z")
  expect_equal(names(DT), c("y", "x", "z"))
  
  set_cols_first(DT, c("x", "y"))
  expect_equal(names(DT), c("x", "y", "z"))
})


test_that("Colums out of intersection", {
  DT <- data.table(y = 1:5, z = 11:15, x = letters[1:5])
  
  expect_error(set_cols_first(DT, c("x", "zz"), intersection = FALSE))
  expect_error(set_cols_last(DT, c("x", "zz"), intersection = FALSE))
})

test_that("Swaps columns appropriate", {
  DT <- data.table(A = 1, B = 2, C = 3)
  expect_equal(names(set_colsuborder(DT, c("C", "B"))), c("A", "C", "B"))
  # Idempotent
  expect_equal(names(set_colsuborder(DT, c("C", "B"))), c("A", "C", "B"))
  DT <- data.table(A = 1, B = 2, C = 3)
  expect_equal(names(set_colsuborder(DT, c("B", "C"))), c("A", "B", "C"))
})
