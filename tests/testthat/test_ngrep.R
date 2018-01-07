context("ngrep")

test_that("Expected output", {
  expect_identical(ngrep("[a-h]", letters), 9:26)
  expect_identical(ngrep("[a-h]", letters, value = TRUE), letters[9:26])
})
