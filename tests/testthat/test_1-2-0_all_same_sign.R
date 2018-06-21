context("all_same_sign")

test_that("all_same_sign", {
  expect_true(all_same_sign(1:10))
  expect_true(all_same_sign(-1 * (1:10)))
  expect_false(all_same_sign(1:10 - 1))
  expect_true(all_same_sign(0))
  expect_true(all_same_sign(NA))
  expect_true(is.na(all_same_sign(c(NA, 1))))
  expect_true(all_same_sign(NULL))
  expect_true(all_same_sign("surprise?"))
  expect_false(xor(0.1 + 0.2 - 0.3 == 0, all_same_sign(c(0, 0.1 + 0.2 - 0.3))))
})
