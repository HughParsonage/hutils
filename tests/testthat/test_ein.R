context("ein")

test_that("Errors in example", {
  skip_if_not_installed("datasets")
  data("iris")
  expect_error(iris[iris$Species %ein% c("setosa", "versicolour"), ])
})

test_that("Errors when expected", {
  expect_error(LETTERS %ein% "a")
  expect_error(LETTERS %enotin% "a")
})

test_that("Identical to in if no error", {
  
  expect_identical(1:5 %in% 1:5, 1:5 %ein% 1:5)
  expect_identical(1:5 %notin% 1:5, 1:5 %enotin% 1:5)
  expect_identical(LETTERS %in% "A", LETTERS %ein% "A")
})