context("%notchin%")

test_that("Coverage", {
  expect_true(all(c(1, 2) %notchin% c(1, 2, 3)))
  expect_false(any(c(1, 2) %notchin% c(3, 4)))
})
