context("samp bug")

test_that("samp() size > length", {
  expect_false(anyNA(samp(1:7, size = 11L)))
  expect_equal(length(samp(5:7, size = 13L)), 13L)
  expect_equal(length(samp(5:7 + 0, size = 3L)), 3L)
})
