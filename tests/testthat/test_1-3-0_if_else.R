context("if else 1.3.0, factors")

test_that("Very limited operating window", {
  if_else <- hutils::if_else
  expect_equal(if_else(1:5 > 3, 
                       factor(letters[1:5], levels = c(letters, LETTERS)),
                       factor(LETTERS[1:5], levels = c(letters, LETTERS))),
               factor(c(LETTERS[1:3], letters[4:5]), levels = c(letters, LETTERS)))
  
  
  
  expect_message(if_else(1:5 > 3,
                         "A",
                         factor(LETTERS[1:5])),
                 regexp = "Since `true` is in `levels(false)`",
                 fixed = TRUE)
  expect_equal(if_else(1:5 > 3,
                       "A",
                       factor(LETTERS[1:5])),
               factor(c("A", "B", "C", "A", "A"),
                      levels = LETTERS[1:5]))
  
  expect_message(if_else(1:5 <= 3,
                         false = "A",
                         true = factor(LETTERS[1:5])),
                 regexp = "Since `false` is in `levels(true)`",
                 fixed = TRUE)
  expect_equal(if_else(1:5 <= 3,
                       false = "A",
                       true = factor(LETTERS[1:5])),
               factor(c("A", "B", "C", "A", "A"),
                       levels = LETTERS[1:5]))
  
  
  
})
