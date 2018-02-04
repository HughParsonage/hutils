context("uniqueLookup")

test_that("Example works", {
  skip_on_cran()
  library(data.table)
  dt <- data.table(
    DateChar = as.character(as.Date("2000-01-01") + sample(10, replace = TRUE, size = 1e6))
  )
  first_time <- system.time(dt[, Date1 := as.Date(DateChar)])
  second_time <- system.time(uniqueLookup(dt, "Date2", "DateChar", as.Date))
  
  expect_ge(first_time[3], second_time[3])
  expect_identical(.subset2(dt, "Date2"),
                   .subset2(dt, "Date1"))
})

test_that("Another example", {
  dt <- data.table(x = sample(paste0(letters, "_Total"), size = 1e5, replace = TRUE),
                   y = 1:1e5)
  
  res <- uniqueLookup(dt, "x", "x", function(x) gsub("_Total", "", x, fixed = TRUE))
  expect_identical(sort(unique(.subset2(dt, "x"))), 
                   letters)
})


