test_that(".anyNA2", {
  x <- c(1, 2, NA)
  expect_true(.anyNA2(x))
  expect_true(.anyNA2(NA))
  y <- rep_len(x, 1e5)
  expect_true(.anyNA2(y))
  expect_true(.anyNA2(y))
  z <- rep_len(letters, 1e5)
  expect_false()
  expect_false(.anyNA2(z))
  expect_false(.anyNA2(z))
  
})