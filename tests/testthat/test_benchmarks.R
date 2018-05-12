context("benchmarking")

test_that("Benchmarks", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  library(microbenchmark)
  invisible(gc(FALSE, reset = TRUE, full = TRUE))
  
  z <- as.logical(1:1e5 %% 3)
  len1e5c <- microbenchmark(if_else(z, "a", rep_len(letters, 1e5)))
  len1e5c_ifelse <- microbenchmark(base::ifelse(z, "a", rep_len(letters, 1e5)))
  expect_lt(median(len1e5c$time) / median(len1e5c_ifelse$time), 0.1)
  
  z <- as.logical(1:1e4 %% 9)
  z[1:1e4 %% 8 == 3] <- NA
  len1e4c <- microbenchmark(if_else(z, "a", rep_len(letters, 1e4)))
  len1e4c_ifelse <- microbenchmark(base::ifelse(z, "a", rep_len(letters, 1e4)))
  expect_lt(median(len1e4c$time), 2e6)
  
  
})


