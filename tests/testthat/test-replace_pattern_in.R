context("test-replace_pattern_in")

test_that("replace_pattern_in works", {
  tempf <- tempfile()
  provide.dir(tempf)
  writeLines(c("aaa", "aa", "a", "ax", "ab"), 
             file.path(tempf, "replace-pattern-in-test.foo"))
  replace_pattern_in("a(?!x)", "", basedir = tempf, file_pattern = "\\.foo$")
  result <- readLines(file.path(tempf, "replace-pattern-in-test.foo"))
  expect_false(any_grepl(result, "a(?!x)"))
  file.remove(file.path(tempf, "replace-pattern-in-test.foo"))
  
  writeLines(c("aaa", "aa", "a", "ax", "ab"), 
             file.path(tempf, "replace-pattern-in-test.R"))
  # fixed
  replace_pattern_in("a", "b", basedir = tempf, 
                     file_contents_fixed = TRUE)
  result <- readLines(file.path(tempf, "replace-pattern-in-test.R"))
  expect_equal(result[1:2], c("bbb", "bb"))
  file.remove(file.path(tempf, "replace-pattern-in-test.R"))
})
