context("mutate ntile")

test_that("character.only = TRUE error handling", {
  library(data.table)
  DT0 <- data.table(x = 1:200, y = rep(1:10, 20L))
  expect_error(mutate_ntile(DT0, col = c("x", "y"), n = 5L, character.only = TRUE),
               regexp = "`col` had length 2",
               fixed = TRUE)
  expect_error(mutate_ntile(DT0, col = 1, n = 5L, character.only = TRUE), 
               regexp = "`col = 1` was a numeric",
               fixed = TRUE)
  expect_error(mutate_ntile(DT0, col = "sdf", n = 5L, character.only = TRUE),
               regexp = "`col = sdf` but this is not a column in `DT`",
               fixed = TRUE)
  DT1 <- data.table(taxableIncome = 1:10)
  expect_error(mutate_ntile(DT1,
                            col = "TaxableIncome",
                            n = 5,
                            character.only = TRUE),
               regexp = "Did you mean `col = taxableIncome`?",
               fixed = TRUE)
  
})

test_that("character.only / NSE", {
  library(data.table)
  DT2 <- data.table(x = 1:200, y = rep(1:10, 20L))
  y <- "x"
  expect_identical(mutate_ntile(DT2, y, n = 5, character.only = TRUE),
                   mutate_ntile(DT2, "x", n = 5, character.only = TRUE))
  
})



test_that("Corresponds to dplyr::ntile", {
  skip_if_not_installed("dplyr")
  library(data.table)
  DT <- data.table(x = 1:101)
  expect_identical(mutate_ntile(DT, x, n = 5)[["xQuintile"]], 
                   dplyr::ntile(1:101, n = 5))
  setkey(DT, x)
  expect_identical(mutate_ntile(DT, x, n = 10)[["xDecile"]], 
                   dplyr::ntile(1:101, n = 10))
  # make uneven
  DT[5L, x := 0L]
  expect_identical(mutate_ntile(DT, x, n = 4)[["xQuartile"]], 
                   dplyr::ntile(1:101, n = 4))
})

test_that("data frames", {
  DT <- data.frame(x = 1:59)
  expect_identical(mutate_ntile(DT, x, n = 10)[["xDecile"]], 
                   dplyr::ntile(1:59, n = 10))
  expect_identical(mutate_ntile(DT, x, n = 1, new.col = "y")[["y"]], 
                   dplyr::ntile(1:59, n = 1))
  DT <- data.frame(xy = rev(1:59))
})






