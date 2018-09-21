context("test-if_else-factor")

test_that("if_else factor symmetric", {
  expect_identical(if_else(1:26 > 3, factor(letters), factor(letters)),
                   factor(letters))
  expect_identical(if_else(1:26 > 0, "a", factor(letters)),
                   factor(rep("a", 26), levels = letters))
  expect_identical(if_else(1:26 < 0, "a", factor(letters)),
                   factor(letters))
  expect_identical(if_else(1:10 > 5,
                           factor("a", levels = letters),
                           factor("b", levels = letters)), 
                   factor(rep(c("b", "a"), each = 5), 
                          levels = letters))
  expect_identical(if_else(c(NA, 1:10 > 5), 
                           factor(letters[1:11]),
                           factor(letters[1:11]),
                           missing = factor("a", levels = letters[1:10])), 
                   factor(letters[c(1:11)]))
  expect_identical(if_else(c(TRUE, FALSE),
                           NA_integer_,
                           factor(c("a", "b"))),
                   factor(c(NA, "b"), levels = c("a", "b")))
  expect_identical(if_else(c(TRUE, FALSE),
                           false = NA_integer_,
                           true = factor(c("a", "b"))),
                   factor(c("a", NA), levels = c("a", "b")))
  
})
