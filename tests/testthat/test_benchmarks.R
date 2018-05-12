context("benchmarking")

test_that("Benchmarks", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  library(microbenchmark)
  invisible(gc(FALSE, reset = TRUE, full = TRUE))
  z <- as.logical(1:10 %% 3)
  ifelse_times <- microbenchmark(ifelse(z, 1, 2), times = 10e3)[["time"]]
  len10 <- microbenchmark(if_else(z, 1, 2), times = 10e3)[["time"]]
  expect_lt(mean(1000 + sort(len10) < sort(ifelse_times)), 0.5)
  
  z <- as.logical(1:1e5 %% 3)
  len10 <- microbenchmark(if_else(z, 1:1e5, 1:1e5))
  expect_lt(median(len10$time), 1e6)
  
  z <- as.logical(1:1e5 %% 3)
  len1e5c <- microbenchmark(if_else(z, "a", rep_len(letters, 1e5)))
  len1e5c_ifelse <- microbenchmark(base::ifelse(z, "a", rep_len(letters, 1e5)))
  expect_lt(median(len10$time), 2e6)
  
  
})

