context("Backwards compatible")

rm_trailing_empty <- function(x) {
  while (!nzchar(x[length(x)])) {
    x <- x[-length(x)]
  }
  x
}

createDigestSha1Tbl <- function(version = c("1.0.0", "1.1.0", "1.2.0", "1.3.0"),
                                return_DT = FALSE) {
  version <- match.arg(version)
  library(data.table)
  DigestSha1 <- function(x) {
    Lines <- readr::read_lines(x)  # encoding more consistent
    substr(digest::sha1(rm_trailing_empty(Lines)), 0, 8)
  }
  Files <- dir(pattern = paste0("^test.", version, ".+\\.R$"))
  DT <- data.table(File = Files)
  DT[, "DigestSha1" := DigestSha1(.BY[["File"]]), by = "File"]
  setcolorder(DT, c("DigestSha1", "File"))
  if (return_DT) {
    return(DT[])
  }
  expected <- fread(dir(path = "version-sha1s",
                        pattern = version,
                        full.names = TRUE)[1],
                    sep = "\t")
  if (!isTRUE(unequal <- all.equal(DT, expected))) {
    print(unequal)
    print(DT)
    print(expected)
    return(FALSE)
  }
  return(TRUE)
}

test_that("1.0.0", {
  skip_if_not_installed("digest")
  skip_if_not_installed("readr")
  skip_if_not(file.exists("test-backwards-compatibility.R"))
  
  expect_true(createDigestSha1Tbl("1.0.0"))
})

test_that("1.1.0", {
  skip_if_not_installed("digest")
  skip_if_not_installed("readr")
  skip_if_not(file.exists("test-backwards-compatibility.R"))
  
  expect_true(createDigestSha1Tbl("1.1.0"))
})

test_that("1.2.0", {
  skip_if_not_installed("digest")
  skip_if_not_installed("readr")
  skip_if_not(file.exists("test-backwards-compatibility.R"))
  
  expect_true(createDigestSha1Tbl("1.2.0"))
})

test_that("1.1.0", {
  skip_if_not_installed("digest")
  tests_1.1.0 <- 
    lapply(dir(pattern = "test_1.1.0.*R$"),
           readLines)
  
  expect_equal(digest::sha1(tests_1.1.0),
               "1f65b68d83c145aeb945699c79c28ac1aa84aabb")
  
})