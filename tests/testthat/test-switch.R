context("test-switch.R")

test_that("Error handling", {
  expect_error(Switch(1:5), 
               "must be a character vector.")
  expect_error(Switch(as.character(1:5)), 
               "DEFAULT.*no default")
  expect_error(Switch(as.character(1:5), DEFAULT = 1:4), 
               "length")
  expect_error(Switch(letters[1:2], DEFAULT = factor("b")), 
               "factor.*not currently supported")
  expect_error(Switch(letters[1:2], a = factor("a"), DEFAULT = "x"), 
               "factor.*not currently supported")
  expect_error(Switch(letters[1:2], a = 1:3, DEFAULT = 1L), 
               "length")
  expect_error(Switch(letters[1:2], a = 1:3, DEFAULT = 0), 
               "type.*double")
})

test_that("length-1 switch", {
  expect_identical(Switch("a", a = 1, b = 2), 
                   switch("a", a = 1, b = 2))
})

test_that("examples", {
  expect_equal(Switch(c("a", "b", "c", "a"), 
                      DEFAULT = 0, 
                      "a" = 1, 
                      "b" = 2, 
                      "c" = 3), 
               c(1:3 + 0, 1))
})

test_that("Multi-length res", {
  expect_equal(Switch(c("a", "b", "c", "a"), 
                      DEFAULT = 0, 
                      "a" = 1, 
                      "b" = 1:4 + 0, 
                      "c" = 3), 
               c(1:3 + 0, 1))
})

test_that("NA", {
  expect_equal(Switch(c(NA, "", "a"), 
                      "a" = "q", 
                      "b", 
                      DEFAULT = "", 
                      IF_NA = "A"), 
               c("A", "b", "q"))
  expect_equal(Switch(c(NA, "", "a", NA), 
                      "a" = "q", 
                      "b", 
                      DEFAULT = "", 
                      IF_NA = LETTERS[1:4]), 
               c("A", "b", "q", "D"))
})

test_that("akin to nested if_else", {
  x <- sample(letters)
  expect_identical(if_else(x == "a", 
                           1:26,
                           if_else(x == "b",
                                   -c(1:26), 
                                   0L), 
                           missing = -99L), 
                   Switch(x, a = 1:26, b = -c(1:26), DEFAULT = 0L, IF_NA = -99L))
})

