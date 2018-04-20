context("%notchin%")

test_that("Coverage", {
  expect_false(all(c(1, 2) %notchin% c(1, 2, 3)))
  expect_true(any(c(1, 2) %notchin% c(3, 4)))
})
