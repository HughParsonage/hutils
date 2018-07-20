context("drop grep")

test_that("drop grep", {
  library(data.table)
  dt <- data.table(x = 1, y = 2)
  expect_equal(drop_grep(dt, "y"), data.table(x = 1))
})
