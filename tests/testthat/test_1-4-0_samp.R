context("samp")

test_that("Error handling", {
  expect_error(samp(0:5, size = "a"),
               regexp = "`size` was a character",
               fixed = TRUE)
  expect_error(samp(0:5, size = 1:2),
               regexp = "`size` had length 2, but must be length-one.",
               fixed = TRUE)
  expect_error(samp(0:5, size = NA_real_),
               regexp = "`size = NA`, but this is not permitted",
               fixed = TRUE)
  expect_error(samp(0:5, size = 2.7),
               regexp = "`size = was type double and could not be safely coerced to integer.",
               fixed = TRUE)
  expect_error(samp(0:5, size = -2L),
               regexp = "`size < 0`, but this is not permitted.",
               fixed = TRUE)
})

test_that("loud", {
  expect_message(samp(5), 
                 regexp ="Using `replace = FALSE`.",
                 fixed = TRUE)
  expect_message(samp(5, replace = TRUE), 
                 regexp = "`length(x) = 1`, so returning `rep.int(x, 1)`.",
                 fixed = TRUE)
})

test_that("Value", {
  res <- samp(5, size = 10)
  # double and just 5's
  expect_identical(res, rep(5, 10))
  res <- samp(1:5, size = 10)
  expect_equal(length(res), 10)
})

test_that("samp length-0", {
  expect_identical(samp(integer(0)), integer(0))
  expect_message(samp(integer(0)))
})


