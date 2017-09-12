# From https://github.com/tidyverse/dplyr/blob/master/tests/testthat/test-if-else.R
# 2017-09-12

context("if_else")

test_that("first argument must be logical", {
  expect_error(if_else(1:10, 1, 2),
               regexp = "`condition` must be a logical vector, but is currently an integer vector",
               fixed = TRUE)
})

test_that("true and false must be same length as condition (or length 1)", {
  expect_error(
    if_else(1:3 < 2, 1:2, 1:3),
    "Only permissible vector lengths are 1 or the maximum of the inputs.",
    fixed = TRUE
  )
  expect_error(
    if_else(1:3 < 2, 1:3, 1:2),
    "Only permissible vector lengths are 1 or the maximum of the inputs.",
    fixed = TRUE
  )
})

test_that("true and false must be same type and same class", {
  expect_error(if_else(1:3 < 2, 1, 1L),
               regexp = "[Tt]ype")
  
  # x <- factor("x")
  # y <- ordered("x")
  # expect_error(
  #   if_else(1:3 < 2, x, y),
  #   "`false` must be factor, not ordered/factor",
  #   fixed = TRUE
  # )
})

test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(if_else(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)
  
  expect_equal(if_else(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(if_else(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(if_else(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})



test_that("better factor support (#2197)", {
  skip("Currently failing")
  
  test_that("gives proper error messages for factor class (#2197)", {
    x <- factor(1:3, labels = letters[1:3])
    
    expect_error(
      if_else(x == "a", "b", x),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", 1L, x),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", 1., x),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", TRUE, x),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", Sys.Date(), x),
      "asdf",
      fixed = TRUE
    )
    
    expect_error(
      if_else(x == "a", x, "b"),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", x, 1L),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", x, 1.),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", x, TRUE),
      "asdf",
      fixed = TRUE
    )
    expect_error(
      if_else(x == "a", x, Sys.Date()),
      "asdf",
      fixed = TRUE
    )
  })
  
  test_that("works with factors as both `true` and `false` (#2197)", {
    x <- factor(1:3, labels = letters[1:3])
    y <- factor(1:3, labels = letters[c(1, 2, 4)])
    
    expect_equal(if_else(x == "a", x[[2]], x), x[c(2, 2, 3)])
    
    expect_error(
      if_else(x == "a", x, y),
      "asdf levels in `false` don't match levels in `true`"
    )
  })
})

# Additional if_else tests:

test_that("Numeric condition", {
  expect_error(if_else(c(0.0, 0.5), 1, 2),
               regexp = "must be a logical.*currently.*double")
})

test_that("Limited yes conditions", {
  expect_error(if_else(TRUE, 1 + 2i, 2),
               regexp = "typeof.*complex")
})

test_that("NA misuse", {
  expect_error(if_else(TRUE, 1L, 1, na = "missing"),
               regexp = "All of yes, no, and na must have the same type")
  expect_error(if_else(TRUE, 1L, 1L, na = "missing"),
               regexp = "All of yes, no, and na must have the same type")
})

test_that("Length-one condition", {
  expect_identical(if_else(TRUE, 1L, 2L), 1L)
  expect_identical(if_else(NA, 1L, 2L), NA_integer_)
  expect_identical(if_else(NA, 1L, 2L, na = 0L), 0L)
})

test_that("Length-one na", {
  expect_identical(if_else(c(TRUE, FALSE), c(1L, 2L), c(3L, 4L), na = 0L), c(1L, 4L))
  expect_identical(if_else(c(TRUE, FALSE, NA), 1L, 2L, na = 0L), c(1L, 2L, 0L))
})

test_that("Multi-length na", {
  expect_identical(if_else(c(TRUE, FALSE, NA), c(1L, 2L, 3L), c(4L, 5L, 6L), na = c(-1L, 0L, 1L)), c(1L, 5L, 1L))
})



