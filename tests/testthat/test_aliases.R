context("Aliases")

test_that("Logicals", {
  expect_true(AND(TRUE, TRUE))
  expect_false(AND(TRUE, FALSE))
  expect_false(AND(FALSE, TRUE))
  expect_false(AND(FALSE, FALSE))
  
  expect_true(OR(TRUE, TRUE))
  expect_true(OR(TRUE, FALSE))
  expect_true(OR(FALSE, TRUE))
  expect_false(OR(FALSE, FALSE))
  
  expect_false(AND(FALSE, stop("Never encountered.")))
  expect_true(OR(TRUE, stop("Never encountered.")))
  
  expect_true(NOR(FALSE, FALSE))
  expect_false(NOR(TRUE, FALSE))
  expect_false(NOR(FALSE, TRUE))
  expect_false(NOR(TRUE, TRUE))
  expect_true(NEITHER(FALSE, FALSE))
  expect_false(NEITHER(TRUE, FALSE))
  expect_false(NEITHER(FALSE, TRUE))
  expect_false(NEITHER(TRUE, TRUE))
  
  xx <- c(TRUE, TRUE, FALSE, FALSE)
  yy <- c(TRUE, FALSE, TRUE, FALSE)
  
  expect_identical(nor(xx, yy), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(neither(xx, yy), nor(xx, yy))
  
  expect_identical(xx %implies% yy, implies(xx, yy))
  expect_identical(xx %implies% yy, c(TRUE, FALSE, TRUE, TRUE))
  expect_error(xx %implies% yy[-1], regexp = "must be the same length")
  
  
})
