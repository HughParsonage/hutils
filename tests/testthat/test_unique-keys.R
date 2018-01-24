context("Unique keys")

test_that("has unique key", {
  library(data.table)
  DT <- data.table(x = 1:5, y = rep("a", 5), key = "x")
  
  expect_true(has_unique_key(DT))
  DT_no_key <- data.table(x = 1:5)
  expect_false(has_unique_key(DT_no_key))
  
  DT_nonunique_key <- data.table(x = 1:5, y = rep("a", 5), key = "y")
  expect_false(has_unique_key(DT_nonunique_key))
  
})

test_that("Set unique key", {
  DT <- data.table(x = 1:5, y = rep("a", 5))
  
  # Main test is that this doesn't error
  set_unique_key(DT, x)
  
  expect_equal(key(DT), "x")
  
  DT <- data.table(x = 1:5, y = rep("a", 5))
  
  expect_error(set_unique_key(DT, y))
  
  
})
