test_that("fst_columns", {
  skip_if_not_installed("fst")
  tempf <- tempfile(fileext = ".fst")
  fst::write_fst(data.frame(xab = 1:10, bcd = 1:10), tempf)
  expect_equal(fst_columns(tempf), c("xab", "bcd"))
  expect_equal(fst_nrow(tempf), 10)
})
