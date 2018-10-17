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






