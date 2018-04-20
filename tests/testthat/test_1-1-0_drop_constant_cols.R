context("Drop constant cols extra")

test_that("Works with <2 rows", {
  expect_equal(drop_constant_cols(data.table(x = 1)), data.table())
  expect_equal(drop_constant_cols(data.frame(x = 1)), data.frame())
})