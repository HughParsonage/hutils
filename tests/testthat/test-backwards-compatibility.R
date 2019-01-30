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
  
  
  if (!isTRUE(all.equal(tests_1.0.0,
                        "fd5fc75ff8d63189c75fd05404b830fe66af5b718ee8e3c1b4d63dd8e2916ae51e594fd973e37590d3aa2113ed2bb276972144faa7f0"))) {
    library(data.table)
    tests_1.0.0.R <- dir(pattern = "test.1.0.0.*\\.R$")
    DT <- data.table(File = tests_1.0.0.R)
    setorderv(DT, "File")  # for C-locale ordering
    DigestSha1 <- function(x) {
      Lines <- readr::read_lines(x)
      substr(digest::sha1(Lines[nzchar(Lines)]), 0, 8)
    }
    print(DT[, "DigestSha1" := DigestSha1(.BY[["File"]]), by = "File"])
    
    
  }
    
})

test_that("1.1.0", {
  skip_if_not_installed("digest")
  tests_1.1.0 <- 
    lapply(dir(pattern = "test_1.1.0.*R$"),
           readLines)
  
  expect_equal(digest::sha1(tests_1.1.0),
               "1f65b68d83c145aeb945699c79c28ac1aa84aabb")
  
})