context("auc")

test_that("Error handling", {
  expect_error(auc(1, 1:2), "unequal lengths")
  expect_error(auc(NA, 1), "contained NAs")
  expect_error(auc(1, NA), "contained NAs")
  
})

test_that("Length-one", {
  expect_equal(auc(actual = TRUE, pred = 0.2), 0.2)
  expect_equal(auc(actual = FALSE, pred = 0.2), 0.8)
  expect_equal(auc(actual = logical(0), pred = double(0)), double(0L))
})

test_that("Prediction and actual can be reversed", {
  expect_equal(auc(c(TRUE, FALSE, TRUE, FALSE), c(0.1, 0.2, 0.3, 0.4)),
               auc(c(0.1, 0.2, 0.3, 0.4), c(TRUE, FALSE, TRUE, FALSE)))
})

test_that("pred integer", {
  expect_equal(auc(actual = c(TRUE, FALSE, TRUE, FALSE), pred = c(0.1, 0.2, 0.3, 0.4)),
               auc(actual = c(1L, 0L, 1L, 0L), pred = c(0.1, 0.2, 0.3, 0.4)))
})

test_that("pred factor", {
  expect_equal(auc(actual = c(TRUE, FALSE, TRUE, FALSE), pred = c(0.1, 0.2, 0.3, 0.4)),
               auc(actual = factor(c(TRUE, FALSE, TRUE, FALSE)), pred = c(0.1, 0.2, 0.3, 0.4)))
  expect_error(auc(actual = factor(1:4), pred = c(0.1, 0.2, 0.3, 0.4)),
               regexp = "but could not be safely mapped to a logical vector as it had 4 values.")
  
  
})

test_that("pred ord factor", {
  expect_equal(auc(actual = c(TRUE, FALSE, TRUE, FALSE), pred = c(0.1, 0.2, 0.3, 0.4)),
               auc(actual = factor(c('a', 'b', 'a', 'b'), 
                                   levels = c("b", "a"),
                                   ordered = TRUE),
                   pred = c(0.1, 0.2, 0.3, 0.4)))
})

test_that("pred double", {
  expect_equal(auc(actual = c(TRUE, FALSE, TRUE, FALSE), pred = c(0.1, 0.2, 0.3, 0.4)),
               auc(actual = c(1, 0, 1, 0), pred = c(0.1, 0.2, 0.3, 0.4)))
})



