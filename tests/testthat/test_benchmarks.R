context("benchmarking")

test_that("Benchmarks", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  library(microbenchmark)
  invisible(gc(FALSE, reset = TRUE, full = TRUE))
  z <- as.logical(1:10 %% 3)
  ifelse_median <- median(microbenchmark(ifelse(z, 1, 2))[["time"]])
  len10 <- microbenchmark(if_else(z, 1, 2))
  len10 <- microbenchmark(if_else(z, 1, 2))
  if (median(len10$time) > ifelse_median) {
    print(len10)
  }
  ifelse_median <- median(microbenchmark(ifelse(z, 1, 2))[["time"]])
  len10 <- microbenchmark(if_else(z, 1, 2))
  len10 <- microbenchmark(if_else(z, 1, 2))
  expect_lt(median(len10$time), ifelse_median)
  
  z <- as.logical(1:1e5 %% 3)
  len10 <- microbenchmark(if_else(z, 1:1e5, 1:1e5))
  expect_lt(median(len10$time), 1e6)
  
  
})

