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

test_that("Errors when integer", {
  expect_error(if_else(1:5 > 3, factor("a"), 1L))
  expect_error(if_else(1:5 > 3, 1L, factor("a")))
})

test_that("Errors", {
  expect_error(if_else(TRUE, factor("a"), factor("b")),
               regexp = "both factors.*different levels")
  expect_error(if_else(FALSE, factor("a"), factor("b")),
               regexp = "both factors.*different levels")
  expect_error(if_else(NA, factor("a"), factor("b"), factor("c")),
               regexp = "both factors.*different levels")
  expect_error(if_else(1:5 > 3, factor("a"), factor("b"), factor("c")),
               regexp = "both factors.*different levels")
  
  expect_error(if_else(TRUE, factor("a"), "b"), regexp = "within the levels")
  expect_error(if_else(FALSE, factor("a"), "b"), regexp = "within the levels")
  expect_error(if_else(NA, factor("a"), "b"), regexp = "within the levels")
  
  expect_error(if_else(TRUE, factor("a"), "b"), regexp = "within the levels")
  expect_error(if_else(FALSE, factor("a"), "b"), regexp = "within the levels")
  expect_error(if_else(NA, factor("a"), "b"), regexp = "within the levels")
  expect_error(if_else(TRUE, false = factor("a"), "b"), regexp = "within the levels of `false`")
  expect_error(if_else(FALSE, false = factor("a"), "b"), regexp = "within the levels of `false`")
  expect_error(if_else(NA, false = factor("a"), "b"), regexp = "within the levels of `false`")
  
  expect_error(if_else(1:7 > 4, factor(letters[1:7]), letters[1:7]), 
               regexp = "`true` is a factor, but `false` is not.",
               fixed = TRUE)
  expect_error(if_else(1:7 > 4, letters[1:7], factor(letters[1:7])), 
               regexp = "`false` is a factor, but `true` is not.",
               fixed = TRUE)
  
  expect_error(if_else(1:7 > 7, "a", "b", factor(letters[1:7])),
               regexp = "`missing` was type factor, but `true` was type character. `missing` must be the same type as `yes`.",
               fixed = TRUE)
  
})


