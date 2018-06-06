context("test-ahull.R")

test_that("Triangles ", {
  ex1 <- ahull(, c(0, 1, 2, 3, 4), c(0, 1, 2, 0, 0))
  expect_equal(ex1$area, 1.5)
  expect_equal(ex1$xmin, 1)
  
  ex2 <- ahull(data.table(x = c(1:6),
                          y = c(0, 2, 1.5, 2, 0.9, -1)))
  expect_equal(ex2$ymax, 1.5)
  
})
