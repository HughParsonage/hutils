context("test-longest_affix")

test_that("Error handling", {
  expect_error(longest_suffix(c(NA, "a"), na.rm = ""),
               regexp = "`na.rm` was type character")
  expect_error(longest_prefix(c(NA, "a"), na.rm = ""),
               regexp = "`na.rm` was type character")
  expect_error(longest_suffix(c(NA, "a"), na.rm = c(TRUE, FALSE)),
               regexp = "`na.rm` was length-2, but must be length-one.",
               fixed = TRUE)
  expect_error(longest_prefix(c(NA, "a"), na.rm = c(TRUE, FALSE)),
               regexp = "`na.rm` was length-2, but must be length-one.",
               fixed = TRUE)
})

test_that("longest prefix", {
  x <- c("aab", "aac", "aad")
  expect_equal(longest_prefix(x), "aa")
  expect_equal(longest_prefix(c(NA, "a", "b"), warn_if_no_prefix = FALSE),
               "")
  expect_warning(longest_prefix(c(NA, "a", "b"), warn_if_no_prefix = TRUE), 
                 regexp = "No common prefix")
  expect_equal(longest_prefix(c(NA, "aba", "aca")), "a")
  expect_equal(longest_prefix(rep("aaa", 4)), "aaa")
})

test_that("longest suffix", {
  expect_equal(longest_suffix(paste0(letters, "total")), "total")
  expect_equal(longest_suffix(c(NA, "a", "b"), warn_if_no_suffix = FALSE), "")
  expect_warning(longest_suffix(c(NA, "a", "b"), warn_if_no_suffix = TRUE),
                 regexp = "No common suffix")
  expect_equal(longest_suffix(c(NA, "aba", "aca")), "a")
  expect_equal(longest_suffix(rep("aaa", 4)), "aaa")
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
  expect_equal(trim_common_affixes(paste0("foo", letters, "babba"),
                                   prefixes = FALSE),
               paste0("foo", letters))
  expect_equal(trim_common_affixes(paste0("foo", letters, "babba"),
                                   suffixes = FALSE),
               paste0(letters, "babba"))

})

test_that("na.rm", {
  expect_equal(longest_prefix(c(NA, "aba", "aca"), na.rm = FALSE), "")
  expect_equal(longest_prefix(c(NA, "aba", "aca"), na.rm = NA), NA_character_)
  expect_equal(longest_suffix(c(NA, "aba", "aca"), na.rm = FALSE), "")
  expect_equal(longest_suffix(c(NA, "aba", "aca"), na.rm = NA), NA_character_)
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

