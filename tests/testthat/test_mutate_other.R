context("Mutate other")

library(data.table)
set.seed(1)
cities <- sample(LETTERS[1:10],
                 size = 1000,
                 replace = TRUE,
                 prob = (10:1) / sum(10:1))

DT <- data.table(City = cities,
                 value = round(rlnorm(1000, log(100))), 
                 key.var = runif(1000))

DT.orig <- copy(DT)

test_that("n = 5", {
  out <- mutate_other(DT, "City", n = 5)[]
  
  letters_in_out <- unique(out$City)
  letters_in_DT <- unique(DT$City)
  
  intersects <- sort(intersect(letters_in_DT, letters_in_out))
  expect_identical(intersects, LETTERS[1:5])
  expect_equal(length(letters_in_out), 5 + 1)
})

test_that("count = 150", {
  out <- mutate_other(DT, "City", count = 150)
  outz <- out[, .N, keyby = "City"]
  expect_true(all(outz[["City"]] > 150))
})

test_that("Order preserved", {
  expect_equal(DT[1][["City"]], "B")
  out <- mutate_other(DT, "City", n = 5)[]
  expect_equal(out[1][["City"]], "B")
  # No change to original
  expect_identical(DT, DT.orig)
})

test_that("n = 5, keyed", {
  DTK <- copy(DT)
  setkey(DTK, key.var)
  
  out <- mutate_other(DTK, "City", n = 5)[]
  
  letters_in_out <- unique(out$City)
  letters_in_DT <- unique(DT$City)
  
  intersects <- sort(intersect(letters_in_DT, letters_in_out))
  expect_identical(intersects, LETTERS[1:5])
  expect_equal(length(letters_in_out), 5 + 1)
  expect_true(haskey(out))
  expect_equal(key(out), "key.var")
  
  # No change to original
  expect_identical(DT, DT.orig)
})

test_that("n = 2", {
  DTn2 <- copy(DT)
  
  expect_error(mutate_other(DTn2, "City", n = 2, copy = FALSE))
  
  out <- mutate_other(DTn2, "City", n = 2, other.category = "Other city")
  
  expect_equal(sort(unique(out[["City"]])), c("A", "C", "Other city"))
  
  # No change to original
  expect_identical(DT, DT.orig)
})

test_that("Warning if not character", {
  expect_warning(mutate_other(DT, var = "value"))
})

test_that("Warning if mass left to default", {
  expect_warning(mutate_other(DT, var = "City", var.weight = "value"))
})

test_that("Mass works as expected", {
  library(data.table)
  library(magrittr)
  
  DT <- data.table(City = c("A", "A", "B", "B", "C", "D"),
                   value = c(1, 9, 4, 4, 5, 11))
  DT.orig <- copy(DT)
  
  expected <-
    data.table(City = c("A", "A", "Other", "Other", "Other", "D"),
               value = c(1, 9, 4, 4, 5, 11))
  
  out <-
    DT %>%
    mutate_other("City", var.weight = "value", mass = 10) %>%
    .[]
  
  expect_identical(out, expected)
  
  expect_identical(DT, DT.orig)
})



