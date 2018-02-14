context("Drop constant cols")

test_that("Error handling", {
  x <- "wontwork"
  expect_error(drop_constant_cols(x),
               regexp = "must be a data.(table|frame)")
  
  expect_warning(drop_constant_cols(data.frame(x = 1:5), copy = FALSE),
                 regexp = "copy")
})

test_that("No constant cols", {
  DT <- data.table(x = 1:5, y = 11:15)
  drop_constant_cols(DT)
  expect_equal(names(DT), c("x", "y"))
})

test_that("Standard use", {
  DT <- data.table(x = c(1, 1), y = c(1, 2))
  drop_constant_cols(DT)
  expect_equal(names(DT), c("y"))
})

test_that("Standard use, copying", {
  DT <- data.table(x = c(1, 1), y = c(1, 2))
  expect_equal(names(drop_constant_cols(DT, copy = TRUE)), 
               c("y"))
  expect_equal(names(DT), c("x", "y"))
})

test_that("No constant cols, data frame", {
  DFi <- data.frame(x = 1:5, y = 11:15)
  DFe <- data.frame(x = 1:5, y = 11:15)
  expect_identical(drop_constant_cols(DFi, copy = TRUE), DFe)
})

test_that("Standard use, data frame, one col returned", {
  DFi <- data.frame(x = rep_len(1, 5), y = 11:15)
  expect_equal(names(drop_constant_cols(DFi, copy = TRUE)), "y")
  expect_true(is.data.frame(drop_constant_cols(DFi, copy = TRUE)))
})


test_that("Indistinct names", {
  DT <- data.table(a = c(2, 3), j = c(1, 1), x = c(1, 1), y = c(1, 2))
  setnames(DT, "x", "y")
  drop_constant_cols(DT)
  expect_equal(ncol(DT), 2L)
  expect_equal(names(DT), c("a", "y"))
  expect_equal(DT[["y"]], c(1, 2))
})

test_that("All columns dropped", {
  DFi <- data.frame(x = rep_len(1, 5), y = rep_len("1", 5))
  expect_equal(nrow(drop_constant_cols(DFi)), nrow(DFi))
  expect_equal(ncol(drop_constant_cols(DFi)), 0)
  
  DT <- as.data.table(DFi)
  drop_constant_cols(DT)
  expect_equal(nrow(DT), 0)
  expect_equal(ncol(DT), 0)
})



