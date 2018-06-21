context("test-ahull.R")

test_that("Triangles ", {
  ex1 <- ahull(, c(0, 1, 2, 3, 4), c(0, 1, 2, 0, 0))
  expect_equal(ex1$area, 1.5)
  expect_equal(ex1$xmin, 1)
  
  ex2 <- ahull(data.table(x = c(1:6),
                          y = c(0, 2, 1.5, 2, 0.9, -1)))
  expect_equal(ex2$ymax, 1.5)
  
})

test_that("Hulls may be above 0 even if the next point on the curve is negative", {
  skip("Not yet considered")
  dt_dip <- data.table(x = 1:11, y = c(1, 2, 1.5, 1.5, 1, 50.1, 50, 1, 4, 2, 1))
  out_dip <- ahull(dt_dip)
  expect_equal(out_dip[["xmin"]], 1)
  expect_equal(out_dip[["ymax"]], 0)
  expect_equal(out_dip[["xmax"]], )
  
  
})
