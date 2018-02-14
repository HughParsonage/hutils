context("Directory")

test_that("Directory created", {
  TempDir <- tempdir()
  provide.dir(file.path(TempDir, "XX"))
  expect_true(dir.exists(file.path(TempDir, "XX")))
  expect_silent(provide.dir(file.path(TempDir, "XX"))) # no error or warning
  expect_true(dir.exists(file.path(TempDir, "XX")))
})
