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
  expect_warning(mutate_other(DT, var = "City", n = NULL, var.weight = "value"))
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
    mutate_other("City", var.weight = "value", mass = 10, n = NULL) %>%
    .[]
  
  expect_identical(out, expected)
  
  expect_identical(DT, DT.orig)
})

test_that("var.weight works as expected", {
  library(data.table)
  library(magrittr)
  library(nycflights13)
  
  
  
  DT <- as.data.table(flights)
  
  flights_by_dest_month <- 
    DT[, .N, keyby = c("month", "dest")]
  
  result <- 
    flights_by_dest_month %>%
    mutate_other("dest", var.weight = "N", n = 5) %>%
    .[]
  
  expect_equal(uniqueN(result[["dest"]]), 5 + 1)
  expect_equal(unique(result[dest != "Other"][["dest"]]),
               c("ATL", "BOS", "LAX", "MCO", "ORD"))
  
  flights_by_dest_month_day <- 
    DT[, .N, keyby = c("month", "dest", "day")]
  
  result_by <-
    flights_by_dest_month_day %>%
    mutate_other("dest", var.weight = "N", by = "month")
  
  
})


test_that("Mutate other weighted", {
  library(nycflights13)
  set.seed(1)
  routes_pax <- 
    as.data.table(flights) %>%
    .[month == 1, .(origin, dest)] %>%
    # random for demonstration
    .[, pax := sample(50:300, size = .N, replace = TRUE)] %>%
    .[]
  
  top5_dests <- 
    routes_pax[, .(tot_pax = sum(pax)), keyby = "dest"][order(-tot_pax)] %>%
    .subset2("dest") %>%
    head
  
  routes_pax_othered <- 
    routes_pax %>%
    mutate_other("dest",
                 other.category = "ZZZ",
                 var.weight = "pax",
                 n = 5) %>%
    .[]

  expect_equal(sort(unique(routes_pax_othered[["dest"]]))[1:5], sort(top5_dests[1:5]))
  expect_equal(sort(unique(routes_pax_othered[["dest"]]))[6], "ZZZ")
  
  
  routes_pax_orig <- routes_pax[, .(tot_pax = sum(pax)), keyby = c("origin", "dest")]
  setorder(routes_pax_orig, origin, -tot_pax)
  
  top_5_dests_JFK <- 
    routes_pax_orig[origin == "JFK"] %>%
    .subset2("dest") %>%
    unique
  
  routes_pax_JFK_othered <- 
    routes_pax %>%
    mutate_other("dest",
                 by = "origin",
                 other.category = "ZZZ",
                 var.weight = "pax",
                 n = 5) %>%
    .[origin == "JFK"]
  
  expect_equal(sort(unique(routes_pax_JFK_othered[["dest"]]))[1:5], sort(top_5_dests_JFK[1:5]))
  expect_equal(sort(unique(routes_pax_JFK_othered[["dest"]]))[6], "ZZZ")
  
})

test_that("Mutate other weighted with mass", {
  library(nycflights13)
  set.seed(1)
  routes_pax <- 
    as.data.table(nycflights13::flights) %>%
    .[month == 1, .(origin, dest)] %>%
    # random for demonstration
    .[, pax := sample(50:300, size = .N, replace = TRUE)] %>%
    .[]
  
  routes_pax_othered <- 
    routes_pax %>%
    mutate_other("dest",
                 other.category = "ZZZ",
                 var.weight = "pax",
                 n = NULL,
                 mass = 2e5) %>%
   .[]
  
  expect_equal(uniqueN(routes_pax_othered[["dest"]]), 7)
  
  
  routes_pax_othered_by <- 
    routes_pax %>%
    mutate_other("dest",
                 other.category = "ZZZ",
                 by = "origin",
                 var.weight = "pax",
                 n = NULL,
                 mass = 1e5) %>%
    .[]
  
  expect_equal(uniqueN(routes_pax_othered_by[["dest"]]), 5)
})

test_that("Corner cases", {
  library(nycflights13)
  set.seed(1)
  flights2 <- as.data.table(nycflights13::flights)
  
  setnames(flights2, "distance", "_temp")
  setnames(flights2, "air_time", "N")
  out <- mutate_other(flights2, "dest")
  expect_equal(uniqueN(out[["dest"]]), 6)
  
  
  flights2 <- as.data.table(nycflights13::flights)
  setnames(flights2, "distance", "wEiGhT")
  expect_error(mutate_other(flights2, var = "tailnum", var.weight = "minute"),
               regexp = "Rename this column (temporarily at least) to use",
               fixed = TRUE)
  setnames(flights2, "wEiGhT", ".rank")
  expect_error(mutate_other(flights2, "tailnum", var.weight = "minute"),
               regexp = "Rename this column (temporarily at least) to use", 
               fixed = TRUE)
})





