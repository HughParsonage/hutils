context("benchmarking")

test_that("Benchmarks", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  skip_if_not_installed("microbenchmark")
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

test_that("selector faster than dt[, .] 10 M", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  skip_if_not_installed("microbenchmark")
  dt <- setDT(list(x = sample.int(1e7), y = rnorm(1e7), z = as.character(runif(1e7))))
  
  library(microbenchmark)
  invisible(gc(FALSE, reset = TRUE, full = TRUE))
  
  selector_0 <- microbenchmark(dt[, .(x, y)])
  selector_2 <- microbenchmark(selector(dt, cols = c("x", "y"), shallow = TRUE))
  expect_gt(median(selector_0$time), 
            median(selector_2$time))
  
  dt <- dt[1:1e5]
  
  selector_0 <- microbenchmark(dt[, .(x, y)])
  selector_1 <- microbenchmark(selector(dt, cols = c("x", "y"), shallow = TRUE))
  
  expect_gt(median(selector_0$time), 
            median(selector_1$time), 
            label = "100,000")
  
  dt <- dt[1:1e4]
  
  selector_0 <- microbenchmark(dt[, .(x, y)])
  selector_1 <- microbenchmark(selector(dt, cols = c("x", "y"), shallow = TRUE))
  
  expect_gt(median(selector_0$time), 
            median(selector_1$time), 
            label = "10,000")
  
  
})

test_that("weight2rows not too much slower than tidyr::uncount", {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("tibble")
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  skip_if_not_installed("microbenchmark")
  library(data.table)
  library(tidyr)
  library(microbenchmark)
  library(hutils)
  DT <- data.table(x = rep_len(sample(500), 2e6))
  DT[, z := rlnorm(.N, 2, 1)]
  DF <- tibble::as_tibble(DT)
  Hutils <- microbenchmark(weight2rows(DT, "z", discard_weight.var = TRUE),
                           times = 20L)
  Tidyr <- microbenchmark(tidyr::uncount(DF, z),
                          times = 20L)
  expect_lt(median(Hutils$time), 
            median(Tidyr$time) * 2,
            label = "weight2rows twice as slow as tidyr::uncount")
  
  
})


