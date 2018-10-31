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
  z <- c("jVDib", "zj7V0", "LrKTN", "q1RJF", "sxEXP", "xS4iF", 
         "RYLlr", "zwH5i", "nMINH", "bbTfK", NA)
  expect_equal(trim_common_affixes(z), z)
  expect_equal(trim_common_affixes(paste0("foo", letters, "babba")), 
               letters)

})

test_that("Corner cases", {
  expect_identical(trim_common_affixes(NULL), character(0))
  expect_identical(trim_common_affixes(c(NULL, "aabbaa", "aaccaa")),
                   c(character(0), "bb", "cc"))
  expect_warning(trim_common_affixes(c("", "a", "b")), 
                 regexp = "No common")
  expect_identical(trim_common_affixes(c(NA, NA, NA_character_)), 
                   c(NA, NA, NA_character_))
})

