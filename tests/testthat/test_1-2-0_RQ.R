context("RQ")

test_that("RQ deparse and normal", {
  expect_false(RQ("hutils", TRUE, FALSE))
  expect_false(RQ(hutils, TRUE, FALSE))
  
  expect_true(RQ("HUTILS", TRUE, FALSE))
  expect_true(RQ(HUTILS, TRUE, FALSE))
  
  expect_error(RQ(c("hutils", "data.table")))
  expect_null(RQ("HUTILS", NULL, stop("Should never be reached")))
})

test_that("isAttached", {
  skip_on_cran()
  expect_true(isAttached(hutils))
  expect_true(isAttached("hutils"))
  expect_false(isAttached("fastmatch"))
  expect_false(isAttached("___ gd9"))
})


