context("test-ahull.R")

test_that("Triangles ", {
  ex1 <- ahull(, c(0, 1, 2, 3, 4), c(0, 1, 2, 0, 0))
  expect_equal(ex1$area, 1.5)
  expect_equal(ex1$xmin, 1)
  
  ex2 <- ahull(data.table(x = c(1:6),
                          y = c(0, 2, 1.5, 2, 0.9, -1)))
  expect_equal(ex2$ymax, 1.5)
  
})

test_that("warnings", {
  expect_warning(ahull(, 0, 0))
})

test_that("mini-utils", {
  expect_identical(A(1, 1, 2, 2, 3, 0),
                   list(0.5, 2.5))
  
  expect_identical(height2x(1.5, 1:5, c(1, 2, -1, 1, 2)),
                   c(2+1/6, 1.5, 4.5))  
  
})

test_that("Corners", {
  h0 <- ahull(, c(0:4), rep(-1, 5))
  expect_equal(h0[["h"]], 0)
  hxy <- ahull(, 0:4, c(0, 1, 2, 1, 0.5))
  expect_equal(hxy[["area"]], 1.75)
})

test_that("Hulls may be above 0 even if the next point on the curve is negative", {
  skip("Not yet considered")
})
