context(".ntile overflow")

test_that(".ntile doesn't overflow", {
  x <- 1:1e7
  expect_false(anyNA(.ntile(x, n = 1000L)))
})

test_that("mutate_ntile doesn't invoke overflow", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("dplyr")
  library(data.table)
  DT <- data.table(x0 = 1:1e7)
  DTM <- mutate_ntile(copy(DT), col = "x0", n = 10e3L, new.col = "TenK")
  setkey(DT, x0)
  DTN <- mutate_ntile(copy(DT), col = "x0", n = 10e3L, new.col = "TenK")
  expect_identical(.subset2(DTM, "TenK"), 
                   .subset2(DTN, "TenK"))
})
