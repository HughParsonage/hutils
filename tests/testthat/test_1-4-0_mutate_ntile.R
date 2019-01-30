context("mutate_ntile 1.4.0")

test_that("trailing key", {
  library(data.table)
  skip_if_not_installed("withr")
  withr::with_seed(seed = 2L, {
    x <- sample(5:15, size = 100, replace = TRUE)
    k1 <- sample(letters, size = 100, replace = TRUE)
    k2 <- round(runif(100))
    k3 <- 1:100 > 3
    DTK <- data.table(x, k1, k2, k3)
    DTJ <- data.table(x, k1, k2, k3)
    setkeyv(DTK, c("k1", "k2", "k3"))
    setorderv(DTJ, c("k1", "k2", "k3"))
    DTJ[, k3Quintile := weighted_ntile(k3, n = 5L)]
    expect_identical(setkey(mutate_ntile(DTK, "k3", n = 5L), NULL),
                     DTJ)
  })
})

# for (RR in dir(path = "R", pattern = "\\.R$")) {
#   dir.create(paste0("timings/", tools::file_path_sans_ext(RR)))
# }

test_that("!is.null(by) coverage", {
  library(data.table)
  DT <- data.table(x = rep(1:4, each = 8),
                   y = c(1:8, 1:8 + 100, 1:8 - 100, 1:8 + 200))
  res <- mutate_ntile(DT, "y", by = "x", n = 4, new.col = "Above")
  expect_equal(res[["Above"]],
               rep(c(1, 1, 2, 2, 3, 3, 4, 4), 4))
})

