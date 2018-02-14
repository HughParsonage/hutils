context("test-1-0-0-major-coalesce.R")

# dplyr coalesce
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

test_that("coalesce replaces 'easy' types", {
  x <- c(NA, 1:5)
  xL <- as.logical(x)
  expect_identical(coalesce(xL, TRUE), rep_len(TRUE, 6L))
  expect_identical(coalesce(x, 0L), 0:5)
  expect_identical(coalesce(as.double(x), 1.5), c(1.5, 1:5))
  expect_identical(coalesce(as.double(x), 1.5), c(1.5, 1:5))
  
})

test_that("non-missing scalar replaces all missing values", {
  x <- c(NA, 1)
  expect_equal(coalesce(x, 1), c(1, 1))
})

test_that("finds non-missing values in multiple positions", {
  x1 <- c(1L, NA, NA)
  x2 <- c(NA, 2L, NA)
  x3 <- c(NA, NA, 3L)
  
  expect_identical(coalesce(x1, x2, x3), 1:3)
})

test_that("Type mismatch", {
  expect_error(coalesce(c(NA, 1:5), as.character(1:6)))
})

test_that("Return x if nothing NA", {
  x <- rnorm(10)
  expect_equal(coalesce(x), x)
  expect_equal(coalesce(x, "Doesn't matter"), x)
})

test_that("Type mismatch", {
  expect_error(coalesce(c(NA, 1:5), as.character(1:6)))
})





