context("RQ")

test_that("RQ deparse and normal", {
  expect_false(RQ("hutils", TRUE, FALSE))
  expect_false(RQ(hutils, TRUE, FALSE))
  
  expect_true(RQ("HUTILS", TRUE, FALSE))
  expect_true(RQ(HUTILS, TRUE, FALSE))
  
  expect_error(RQ(c("hutils", "data.table")))
  expect_null(RQ("hutils", TRUE, stop("Should never be reached")))
})


