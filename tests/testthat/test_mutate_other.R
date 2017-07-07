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

test_that("n = 5", {
  out <- mutate_other(DT, "City", n = 5)[]
  
  letters_in_out <- unique(out$City)
  letters_in_DT <- unique(DT$City)
  
  intersects <- sort(intersect(letters_in_DT, letters_in_out))
  expect_identical(intersects, LETTERS[1:5])
  expect_equal(length(letters_in_out), 5 + 1)
})

test_that("Order preserved", {
  expect_equal(DT[1][["City"]], "B")
  out <- mutate_other(DT, "City", n = 5)[]
  expect_equal(out[1][["City"]], "B")
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
})

test_that("n = 2", {
  DTn2 <- copy(DT)
  
  expect_error(mutate_other(DTn2, "City", n = 2, copy = FALSE))
  
  out <- mutate_other(DTn2, "City", n = 2, other.category = "Other city")
  
  expect_equal(sort(unique(out[["City"]])), c("A", "C", "Other city"))
  
  
})

test_that("Warning if not character", {
  expect_warning(mutate_other(DT, var = "value"))
})



