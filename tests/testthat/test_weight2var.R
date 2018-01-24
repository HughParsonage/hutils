context("weight2rows")

test_that("Example works", {
  # Don't use 2 in y! 
  DT <- data.table(x = 1:5, y = as.integer(c(1, 1, 1, 1, 3)))
  out <- weight2rows(DT, "y")
  expect_equal(sum(out[["x"]]), sum(DT[["x"]] * DT[["y"]]))
  expect_equal(nrow(out), sum(DT[["y"]]))
})

test_that("Double variable works", {
  DT <- data.table(x = 1:10e3, weight = rlnorm(10e3, meanlog = 5))
  out <- weight2rows(DT, "weight")
  error <- sum(DT[["weight"]]) - sum(floor(DT[["weight"]]))
  expect_equal(nrow(out), sum(DT[["weight"]]), tol = error)
})

test_that("Logical variable works with a warning", {
  DT <- data.table(x = 1:4, yes = c(TRUE, TRUE, FALSE, FALSE))
  out <- suppressWarnings(weight2rows(DT, "yes"))
  expect_warning(weight2rows(DT, "yes"))
  expect_equal(out[["x"]], 1:2)
})

test_that("Data frame", {
  DT <- data.frame(x = 1:5, y = as.integer(c(1, 1, 1, 1, 3)))
  out <- weight2rows(DT, "y")
  expect_equal(sum(out[["x"]]), sum(DT[["x"]] * DT[["y"]]))
  expect_equal(nrow(out), sum(DT[["y"]]))
})

test_that("Error when negative weight", {
  expect_error(weight2rows(data.table(x = 1:4, sdgoih = c(-1, 1, 1, 1)), "sdgoih"),
               regexp = "`weight.var` contains negative values.",
               fixed = TRUE)
})
