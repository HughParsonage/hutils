test_that("Issue 40, longest_suffix", {
  expect_equal(longest_suffix(c("B", "CB", "ACB")), "B")
  expect_equal(longest_prefix(c("B", "BC", "BCA")), "B")
})
