context("select_which")

test_that("Returns correct columns", {
  library(data.table)
  DT <- data.table(x = 1:5,
                   y = letters[1:5],
                   AB = c(NA, TRUE, FALSE))
  out <- select_which(DT, is.numeric)
  expect_equal(names(out), "x")
  out <- select_which(DT, anyNA, .and.dots = "y")
  expect_equal(names(out), c("AB", "y"))
  
  expect_error(select_which(data.frame(x = 1), checkDT = TRUE), regexp = "must be a data.table")
})
