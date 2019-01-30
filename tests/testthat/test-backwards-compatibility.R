context("Backwards compatible")

test_that("1.0.0", {
  skip_if_not_installed("digest")
  skip_if_not(file.exists("test-backwards-compatibility.R"))
  tests_1.0.0 <- 
    lapply(dir(pattern = "test_1.0.0.*R$"),
           readLines,
           # incomplete final line found on 'test_1-0-0-major-drop_colr.R'
           warn = FALSE)
  
  expect_equal(digest::sha1(tests_1.0.0),
               "098a575b17ff0629aa03bda1500e26abbc69dfd6")
    
})

test_that("1.1.0", {
  skip_if_not_installed("digest")
  tests_1.1.0 <- 
    lapply(dir(pattern = "test_1.1.0.*R$"),
           readLines)
  
  expect_equal(digest::sha1(tests_1.1.0),
               "1f65b68d83c145aeb945699c79c28ac1aa84aabb")
  
})