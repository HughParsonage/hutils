context("number2word")

test_that("Basics", {
  expect_equal(number2word(5), "five")
  expect_equal(number2word(55), "fifty-five")
  expect_equal(number2word(14), "fourteen")
  expect_equal(number2word(41), "forty-one")
  expect_equal(number2word(90), "ninety")
  expect_equal(number2word(0), "zero")
  expect_equal(number2word(0, zero = ""), "")
  expect_equal(number2word(100), "one hundred")
  expect_equal(number2word(2590L), "two thousand, five hundred and ninety")
})

context("word2number")

test_that("Basics", {
  expect_equal(word2number("five"), 5)
  expect_equal(word2number("one hundred and sixty"), 160)
  expect_equal(word2number("five hundred and forty-three thousand and forty-three"),
               543043)
  expect_equal(word2number(c("five hundred and forty", "five thousand, two hundred")),
               c(540, 5200))
})

test_that("Long", {
  expect_equal(word2number(rep_len("one thousand three hundred and sixteen", 101)),
               rep_len(1316, 101))
})

test_that("millions", {
  input <- c("three hundred and four million, two hundred and seventeen thousand and twenty-four",
             "ninety-four",
             "ninety-four thousand")
  expect_equal(word2number(input),
               c(304*1e6+217e3+24,
                 94,
                 94000))
})

test_that(".thousands2Expr", {
  expect_equal(.thousands2Expr(""), '0')
})



