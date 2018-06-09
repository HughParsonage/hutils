context("aliases")

test_that("XOR", {
  expect_equal(XOR(TRUE, TRUE), FALSE)
  expect_equal(XOR(TRUE, FALSE), TRUE)
  expect_equal(XOR(TRUE, NA), NA)
  expect_equal(XOR(FALSE, TRUE), TRUE)
  expect_equal(XOR(FALSE, FALSE), FALSE)
  expect_equal(XOR(FALSE, NA), NA)
  expect_equal(XOR(NA, TRUE), NA)
  expect_equal(XOR(NA, FALSE), NA)
  expect_equal(XOR(NA, NA), NA)
})
