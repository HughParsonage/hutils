context("test-longest_affix")

test_that("longest prefix", {
  x <- c("aab", "aac", "aad")
  expect_equal(longest_prefix(x), "aa")
})

test_that("longest suffix", {
  expect_equal(longest_suffix(paste0(letters, "total")), "total")
})

test_that("trimming", {
  y <- paste(LETTERS, 1:26)
  expect_equal(trim_common_affixes(paste0("QQQ**", y, "(BBB)")),
               y)

})

