context("if else 1.2.0, issue 14")

test_that("Error message refers to length(condition), not max.length", {
  expect_error(if_else(logical(0), 1:5, 1:5), 
               regexp = "`true` must have the same length as `condition`",
               fixed = TRUE)
  expect_error(if_else(logical(0), 1L, 1:5), 
               regexp = "`false` must have the same length as `condition`",
               fixed = TRUE)
  expect_error(if_else(logical(0), 1L, 2L, missing = 1:5), 
               regexp = "`missing`.* must have the same length as `condition`")
  
  # Made this mistake
  expect_error(if_else(logical(0), 1L, 2L, missing = 1:5), 
               regexp = "`missing` had length 5",
               fixed = TRUE)
})
