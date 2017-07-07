context("Duplicated rows")

test_that("Expected results", {
  DT <- data.table(x = c(1, 1, 2, 2, 1),
                   y = c(1, 2, 2, 2, 1),
                   z = c(1, 2, 3, 4, 5))
  
  expect_equal(nrow(duplicated_rows(DT)), 0)
  
  DT_expected <- data.table(x = c(1, 2, 2, 1),
                            y = c(1, 2, 2, 1), 
                            z = c(1, 3, 4, 5))
  
  expect_equal(duplicated_rows(DT, by = c("x", "y"), order = FALSE), DT_expected)
})

test_that("na.rm", {
  DT <- data.table(x = c(1, 1, 2, 2, 1, NA, NA),
                   y = c(1, 2, 2, 2, 1, NA, NA),
                   z = c(1, 2, 3, 4, 5, 6, 7))
  
  DT_narm_TRUE <- data.table(x = c(1, 2, 2, 1),
                             y = c(1, 2, 2, 1), 
                             z = c(1, 3, 4, 5))
  
  expect_equal(duplicated_rows(DT, by = c("x", "y"), order = FALSE, na.rm = TRUE), 
               DT_narm_TRUE)
  
  DT_narm_FALSE <- data.table(x = c(1, 2, 2, 1, NA, NA),
                              y = c(1, 2, 2, 1, NA, NA), 
                              z = c(1, 3, 4, 5, 6, 7))
  
  expect_equal(duplicated_rows(DT, by = c("x", "y"), order = FALSE, na.rm = FALSE),
               DT_narm_FALSE)
})

test_that("copy", {
  DT <- data.table(x = c(1, 1, 2, 2, 1, NA, NA),
                   y = c(1, 2, 2, 2, 1, NA, NA),
                   z = c(1, 2, 3, 4, 5, 6, 7))
  expect_equal(nrow(duplicated_rows(DT, by = c("x", "y", "z"), copyDT = FALSE)), 0)
  
  expect_equal(DT[["z"]], c(6, 7, 1, 5, 2, 3, 4))
  
  expect_equal(nrow(duplicated_rows(DT, by = c("x", "y", "z"), copyDT = FALSE, na.last = TRUE)), 0)
  
  expect_equal(DT[["z"]], c(1, 5, 2, 3, 4, 6, 7))
})
