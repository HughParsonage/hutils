context("test-select_grep.R")

test_that("Error handling", {
  library(data.table)
  expect_error(select_grep(list(x = 1)), regexp = "data\\.frame")
  expect_error(select_grep(data.table(x = 1)),
               regexp = "`patterns` is missing or length-zero, with no default.",
               fixed = TRUE)
  expect_error(select_grep(data.table(x = 1),
                           patterns = 1),
               regexp = "`patterns` is not a character vector.",
               fixed = TRUE)
})

test_that("Select pattern", {
  library(data.table)
  DT <- data.table(x1 = 1, x2 = 2, x3 = 3)
  expect_equal(names(select_grep(DT, "2")), "x2")
  expect_equal(names(select_grep(DT, "[12]")), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"))), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"), .and = "x2")), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"), .and = "x3")), c("x1", "x2", "x3"))
  expect_equal(names(select_grep(DT, c("1"), .and = "x3", .but.not = 3)), c("x1"))
  expect_equal(names(select_grep(DT, c("1"), .and = "x3", .but.not = c(FALSE, FALSE, FALSE))),
               c("x1", "x3"))
  expect_equal(names(select_grep(DT, c("1"), .and = "x3", .but.not = c(TRUE, FALSE, FALSE))),
               "x3")
  expect_equal(names(select_grep(DT, c("1"), .and = "x3", .but.not = "x3")),
               "x1")
  
})

test_that("Works for non-data.table data.frames", {
  DT <- data.frame(x1 = 1, x2 = 2, x3 = 3)
  expect_equal(names(select_grep(DT, "2")), "x2")
  expect_equal(names(select_grep(DT, "[12]")), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"))), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"), .and = "x2")), c("x1", "x2"))
  expect_equal(names(select_grep(DT, c("1", "2"), .and = "x3")), c("x1", "x2", "x3"))
  expect_equal(names(select_grep(DT, c("1"), .and = "x3", .but.not = 3)), c("x1"))
})

test_that("Coverage", {
  library(data.table)
  DT <- data.table(x = 1, y = 2)
  expect_identical(select_grep(DT, patterns = ".", .but.not = 2L), data.table(x = 1))
  expect_identical(select_grep(DT, patterns = "x", .and = c(FALSE, TRUE, FALSE)), DT)
  expect_identical(select_grep(DT, patterns = "x", .and = 2L), DT)
  expect_identical(select_grep(DT, patterns = "x", .and = 2), DT)
  expect_identical(select_grep(DT, patterns = "x", .but.not = 1), data.table())
})
