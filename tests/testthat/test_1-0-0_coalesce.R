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

context("coalesce")

test_that("error if invalid length", {
  expect_error(
    coalesce(c(NA_integer_, 2), 1:3),
    "only permissible lengths in ...",
    fixed = TRUE
  )
})

test_that("Factor mismatch", {
  expect_error(coalesce(c(NA, 1:5), factor(1:6)), 
               regexp = "Argument 2 was a factor, but `x` was not.", 
               fixed = TRUE)
})
