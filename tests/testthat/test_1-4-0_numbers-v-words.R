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
})

context("word2number")

test_that("Basics", {
  expect_wqual(word2number("five"), 5)
  expect_wqual(word2number("one hundred and sixty"), 160)
})
