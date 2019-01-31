context("Backwards compatible")

test_that("1.0.0", {
  skip_if_not_installed("digest")
  skip_if_not_installed("readr")
  skip_if_not(file.exists("test-backwards-compatibility.R"))
  tests_1.0.0 <- 
    vapply(dir(pattern = "test.1.0.0.*\\.R$"),
           tools::md5sum,
           "") %>%
    vapply(substr, 1L, 4L, FUN.VALUE = "") %>%
    paste0(collapse = "")
  
  rm_trailing_empty <- function(x) {
    while (!nzchar(x[length(x)])) {
      x <- x[-length(x)]
    }
    x
  }
  
  library(data.table)
  tests_1.0.0.R <- dir(pattern = "test.1.0.0.*\\.R$")
  DT <- data.table(File = tests_1.0.0.R)
  setorderv(DT, "File")  # for C-locale ordering
  DigestSha1 <- function(x) {
    Lines <- readr::read_lines(x)  # encoding more consistent
    substr(digest::sha1(rm_trailing_empty(Lines)), 0, 8)
  }
  DT[, "DigestSha1" := DigestSha1(.BY[["File"]]), by = "File"]
  setcolorder(DT, c("DigestSha1", "File"))
  expected <- fread("version-sha1s/v1-0-0.tsv", sep = "\t")
  # Provide better diagnostics
  if (!isTRUE(v100_equal <- all.equal(DT, expected))) {
    print(v100_equal)
    print(DT)
    print(expected)
    expect_true(FALSE)
  }
  expect_true(TRUE)
})

test_that("1.1.0", {
  skip_if_not_installed("digest")
  tests_1.1.0 <- 
    lapply(dir(pattern = "test_1.1.0.*R$"),
           readLines)
  
  expect_equal(digest::sha1(tests_1.1.0),
               "1f65b68d83c145aeb945699c79c28ac1aa84aabb")
  
})