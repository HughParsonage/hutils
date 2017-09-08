context("weight2var")

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

