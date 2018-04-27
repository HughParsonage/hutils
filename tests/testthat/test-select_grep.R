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
  
  expect_error(select_grep(DT, patterns = ".", perl = NA),
               regexp = "`perl` had a value other than TRUE or FALSE")
  expect_error(select_grep(DT, patterns = ".", perl = NA, fixed = NA),
               regexp = "The following arguments.*had a value other than TRUE or FALSE")
  expect_warning(select_grep(DT, patterns = ".", ignore.case = TRUE, fixed = TRUE),
                 regexp = "Changing argument `ignore.case` to FALSE since ")
  expect_equal(ncol(select_grep(DT, patterns = ".", ignore.case = TRUE, fixed = TRUE, 
                                .warn.fixed.mismatch = FALSE)),
               0)
  
  for (k in LETTERS) {
    DT[, (k) := NA]
  }
  expect_warning(select_grep(DT, patterns = ".", ignore.case = TRUE, fixed = TRUE),
                 regexp = "(first 6 shown)",
                 fixed = TRUE)
})

test_that("Arguments passed to grep", {
  library(data.table)
  DT <- data.table(xy = 1, yz = 2, x. = 4)
  expect_warning(select_grep(DT, "x", perl = TRUE, fixed = TRUE))
  expect_equal(names(select_grep(DT, "x.", perl = FALSE, fixed = TRUE)), "x.")
  
  DT <- data.table(xy = 1, XY = 2)
  expect_equal(ncol(select_grep(DT, "xy", ignore.case = TRUE)), 2)
  expect_equal(ncol(select_grep(DT, "xy", fixed = TRUE, perl = FALSE)), 1)
  expect_equal(ncol(select_grep(DT, "xy", perl = FALSE, ignore.case = TRUE)), 2)
  expect_equal(ncol(select_grep(DT, "xy", perl = FALSE, ignore.case = TRUE, fixed = TRUE, 
                                .warn.fixed.mismatch = FALSE)),
               2)
  expect_warning(select_grep(DT, "xy", perl = FALSE, ignore.case = TRUE, fixed = TRUE),
                 regexp = "since `fixed = TRUE`")
  
  DT <- data.table(xy = 1, x.y = 2, xzy = 3)
  expect_warning(select_grep(DT, "x.y", perl = TRUE, ignore.case = TRUE, fixed = TRUE), 
                 regexp = "since `fixed = TRUE`.*opposite was intended.*xzy")
  expect_warning(select_grep(DT, "x.y", perl = TRUE, ignore.case = FALSE, fixed = TRUE), 
                 regexp = "since `fixed = TRUE`.*opposite was intended.*xzy")
  
  
})

test_that("Multi-length patterns", {
  library(data.table)
  DT <- data.table(x = 1, y = 2)
  expect_equal(select_grep(DT, c("x", "y"), fixed = TRUE), DT)
  DT <- data.table(x = 1, Y = 2)
  expect_equal(select_grep(DT,
                           patterns = c("x", "y"),
                           fixed = TRUE,
                           ignore.case = FALSE, 
                           .warn.fixed.mismatch = FALSE),
               data.table(x = 1))
  expect_warning(select_grep(DT, c("x", "y"), fixed = TRUE, ignore.case = TRUE), 
                 regexp = "This can lead to wrong columns being selected or dropped.")
  expect_warning(select_grep(DT, c("x", "y"), fixed = TRUE, ignore.case = TRUE, perl = TRUE), 
                 regexp = "This can lead to wrong columns being selected or dropped.")
  expect_equal(select_grep(DT, c("x", "y"), fixed = TRUE, ignore.case = TRUE,
                           .warn.fixed.mismatch = FALSE),
               DT)
})


