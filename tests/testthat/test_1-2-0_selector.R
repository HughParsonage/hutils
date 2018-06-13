context("selector")

test_that("Expected results", {
  dt <- data.table(x = 1, y = 2, z = 3, key = "z")
  o1 <- selector(dt, x, y)
  o2 <- selector(dt, cols = c("x", "y"))
  o3 <- selector(dt, x, cols = "y")
  o4 <- selector(dt, y, cols = "x")
  expect_equal(o1, dt[, .(x, y)])
  expect_equal(o2, dt[, .(x, y)])
  expect_equal(o3, dt[, .(x, y)])
  expect_equal(o4, dt[, .(y, x)])
  expect_false(haskey(o4))
  o5 <- selector(dt, z, x)
  expect_equal(o5, dt[, .(z, x)])
  expect_true(haskey(o5))
  expect_equal(key(o5), "z")
  
  o6 <- selector(dt, cols = 1:2)
  expect_equal(o6, dt[, .(x, y)])
  
  o0 <- selector(dt)
  expect_identical(o0, data.table())
})
