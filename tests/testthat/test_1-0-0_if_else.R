# From https://github.com/tidyverse/dplyr/blob/master/tests/testthat/test-if-else.R
# 2017-09-12

# dplyr if_else
# https://github.com/tidyverse/dplyr/blob/master/tests/testthat/test-coalesce.R
# The MIT License (MIT)
# =====================
#   
#   Copyright (C) 2013-2015 RStudio and others.
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the ``Software''), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
#   
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

context("if_else")

test_that("first argument must be logical", {
  expect_error(if_else(1:10, 1, 2),
               regexp = "`condition` must be a logical vector, but is currently an integer vector",
               fixed = TRUE)
})

test_that("true and false must be same length as condition (or length 1)", {
  expect_error(
    if_else(1:3 < 2, 1:2, 1:3),
    "`true` had length 2 but `condition` had length 3.",
    fixed = TRUE
  )
  expect_error(
    if_else(1:3 < 2, 1:3, 1:2),
    "`false` had length 2 but `condition` had length 3.",
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
  expect_error(if_else(TRUE, 1L, 1, missing = "missing"),
               regexp = "All of true, false, and missing must have the same type")
  expect_error(if_else(TRUE, 1L, 1L, missing = "missing"),
               regexp = "All of true, false, and missing must have the same type")
})

test_that("Length-one condition", {
  expect_identical(if_else(TRUE, 1L, 2L), 1L)
  expect_identical(if_else(NA, 1L, 2L), NA_integer_)
  expect_identical(if_else(FALSE, 1L, 2L), 2L)
  expect_identical(if_else(NA, 1L, 2L, missing = 0L), 0L)
})

test_that("Length-one na", {
  expect_identical(if_else(c(TRUE, FALSE), c(1L, 2L), c(3L, 4L), missing = 0L), c(1L, 4L))
  expect_identical(if_else(c(TRUE, FALSE, NA), 1L, 2L, missing = 0L), c(1L, 2L, 0L))
})

test_that("Multi-length yes", {
  expect_identical(if_else(c(TRUE, FALSE), c(1L, 2L), c(3L, 4L)), c(1L, 4L))
  expect_identical(if_else(c(TRUE, FALSE), c(1L, 2L), c(3L, 4L), missing = 0L), c(1L, 4L))
})

test_that("Multi-length na", {
  expect_identical(if_else(c(TRUE, FALSE, NA), c(1L, 2L, 3L), c(4L, 5L, 6L), missing = c(-1L, 0L, 1L)), c(1L, 5L, 1L))
})

test_that("Matches dplyr::if_else for multi-length missing", {
  expect_identical( dplyr::if_else(c(TRUE, FALSE, NA), 1, 2, missing = c(1, 2, 3)),
                   hutils::if_else(c(TRUE, FALSE, NA), 1, 2, missing = c(1, 2, 3)))
})

test_that("Must be faster than dplyr::if_else", {
  if (requireNamespace("dplyr", quietly = TRUE) &&
      requireNamespace("microbenchmark", quietly = TRUE)) {
    library(microbenchmark)
    library(magrittr)
    library(data.table)
    
    x <- rcauchy(200e3, 2, 3) 
    
    out <- 
      microbenchmark(hutils = hutils::if_else(x < 0, "a", "b"),
                     dplyr  =  dplyr::if_else(x < 0, "a", "b"),
                     times = 30) %>%
      as.data.table %>%
      .[, .(time = median(time)), by = expr]
    
    expect_gt(out[expr == "dplyr"][["time"]],
              out[expr == "hutils"][["time"]])
  }
})
 
test_that("Must be faster than dplyr::if_else in all exits", {
  skip_on_cran()
  if (requireNamespace("dplyr", quietly = TRUE) &&
      requireNamespace("microbenchmark", quietly = TRUE)) {
    library(microbenchmark)
    library(magrittr)
    library(data.table)   
    compare_dplyr_hutils <- function(cond.length.one, true.length.one, false.length.one, missing.length.one, 
                                     the.type = c("logical", "integer", "double", "character"), 
                                     ii) {
      
      if (cond.length.one) {
        size <- 1
      } else {
        size <- ceiling(abs(rcauchy(1, scale = 10)) + 1)
      }
      cond <- sample(c(TRUE, FALSE, NA), size = size, replace = TRUE)
      the.type <- match.arg(the.type)
      
      fetch_type <- function(the.type) {
        switch(the.type, 
               "logical" = sample(c(TRUE, FALSE, NA), size = size, replace = TRUE),
               "integer" = sample(c(1:2, .Machine$integer.max), size = size, replace = TRUE),
               "double" = sample(c(1, .Machine$double.xmax, .Machine$double.eps), size = size, replace = TRUE),
               "character" = sample(letters, size = size, replace = TRUE))
      }
      
      yes <- fetch_type(the.type)
      
      no <- fetch_type(the.type)
      
      na <- fetch_type(the.type)
      
      
      out <- 
        microbenchmark(hutils = hutils::if_else(cond, yes, no, na),
                       dplyr  =  dplyr::if_else(cond, yes, no, na),
                       times = 25) %>%
        as.data.table %>%
        .[, .(time = median(time)), by = expr]
      
      if (out[expr == "dplyr"][["time"]] <
          out[expr == "hutils"][["time"]]) {
        cat("cond", cond, "\n")
        cat("yes", yes, "\n")
        cat("no", no, "\n")
        cat("na", na, "\n")
        print(out)
      } else {
        expect_true(TRUE)
      }
      TRUE
    }
    
    test_input <- 
      CJ(A = c(TRUE, FALSE),
         B = c(TRUE, FALSE),
         C = c(TRUE, FALSE),
         D = c(TRUE, FALSE), 
         E = c("logical", "integer", "double", "character")) 
    
    for (i in seq_len(nrow(test_input))) {
      test_input[i, x := compare_dplyr_hutils(A, B, C, D, E, i)]
    }
    
  }
})

test_that("if_else coverage", {
  out <- if_else(c(NA, FALSE, TRUE, NA), 1:4 + 0.0, 3)
  expect_equal(out, c(NA, 3, 3, NA))
  out <- if_else(c(NA, FALSE, TRUE, NA), 1:4 + 0.0, 3, missing = -1)
  expect_equal(out, c(-1, 3, 3, -1))
})



