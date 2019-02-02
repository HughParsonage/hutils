context("Mode")

test_that("Mode works on logical", {
  expect_equal(Mode(c(TRUE, TRUE, FALSE)), TRUE)
  expect_equal(Mode(c(TRUE, TRUE, FALSE, NA)), TRUE)
  expect_equal(Mode(c(TRUE, FALSE, FALSE)), FALSE)
  expect_equal(Mode(c(TRUE, FALSE, FALSE, NA)), FALSE)
  expect_equal(Mode(c(TRUE, FALSE, NA, NA)), NA)
})

test_that("Mode works", {
  expect_equal(Mode(c(letters, "a")), "a")
  expect_equal(Mode(c(1:10, 5L)), 5L)
  expect_equal(Mode(c(1:10, 5)), 5)
  
  # Multimodal 
  expect_true(any(c(5, 6) %in% Mode(c(5, 5, 6, 6)), na.rm = TRUE))
})

test_that("Mode length-0", {
  expect_identical(Mode(integer(0)), integer(0))
})

test_that("Other", {
  expect_identical(Mode(raw(5)), raw(5)[1])
})

