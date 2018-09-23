context("isTrueFalse")

test_that("check_TF", {
  mm <- 1
  expect_error(check_TF(mm), regexp = "`mm` was type .*double")
  ll <- c(TRUE, FALSE)
  expect_error(check_TF(ll), regexp = "`ll` had length 2 but must be length-one")
  nn <- NA
  expect_error(check_TF(nn), regexp = "`nn = NA` but must be TRUE or FALSE.")
  expect_null(check_TF(TRUE))
  expect_null(check_TF(FALSE))
})


