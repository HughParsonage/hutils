context("test-ahull.R")

test_that("Triangles ", {
  ex1 <- ahull(, c(0, 1, 2, 3, 4), c(0, 1, 2, 0, 0))
  expect_equal(ex1$area, 1.5)
  expect_equal(ex1$xmin, 1)
  
  ex2 <- ahull(data.table(x = c(1:6),
                          y = c(0, 2, 1.5, 2, 0.9, -1)))
  ex3 <- ahull(data.table(x = c(1:6),
                          y = -c(0, 2, 1.5, 2, 0.9, -1)),
               incl_negative = TRUE)
  ex4 <- ahull(data.table(x = c(1:6),
                          y = c(0, 2, 1.5, 2, 0.9, -1)),
               maximize = "")
  expect_equal(ex2$ymax, 1.5)
  expect_equal(ex3$ymin, -1.5)
  
  ex5 <- ahull(data.table(x = 1:5, 
                          y = c(0, 1, 0, 1, 0)))
  expect_equal(ex5[["h"]], 0)
  expect_true(is.na(area_from_min(1L, data.table(x = 1:6, y = c(0, 1, 0, 1, 0, 0)))[["xmax"]]))
  expect_equal(area_from_min(1L, data.table(x = 1:6, y = c(0, 1, 0, 1, 0, 1)))[["h"]], 0)
  expect_equal(area_from_min(6L, data.table(x = 1:6, y = c(0, 1, 0, 1, 0, 0)))[["h"]], 0)
  expect_equal(area_from_min(2L, data.table(x = 1:6, y = c(0, 1, 0, 1, 0, 0)))[["h"]], 1)
  expect_true(is.na(area_from_min(2L, data.table(x = 1:6, y = c(0, 1, 0, 1, 0, 0)))[["w"]]))
  expect_true(is.na(area_from_min(4L, data.table(x = 1:6, y = c(0.1, 1, 0, 1, 0.2, 0)))[["w"]]))
  
})

test_that("warnings", {
  expect_warning(ahull(, 0, 0))
})

test_that("mini-utils", {
  library(magrittr)
  library(data.table)
  expect_identical(A(1, 1, 2, 2, 3, 0),
                   list(0.5, 2.5))
  
  expect_identical(height2x(1.5, 1:5, c(1, 2, -1, 1, 2)),
                   c(2+1/6, 1.5, 4.5))
  expect_error(height2x(1.5, 5:1, c(1, 2, -1, 1, 2)), 
               regexp = "sorted")
  
  dtemi <- data.table(x = 1:5,
                      y = c(0.03, 0.49, 0, 0.65, 1))
  expect_identical(areas_right_of(dtemi),
                   c(1L, 3L))
  dtemil_1nna <- 
    areas_right_of(dtemi, return_ind = FALSE)[[1L]] %>%
    as.double %>%
    .[!is.na(.)]
  expect_equal(dtemil_1nna, 0.23, tol = 0.001)
  
  
})

test_that("Corners", {
  h0 <- ahull(, c(0:4), rep(-1, 5))
  expect_equal(h0[["h"]], 0)
  hxy <- ahull(, 0:4, c(0, 1, 2, 1, 0.5))
  expect_equal(hxy[["area"]], 1.75)
  h01 <- ahull(, c(0:4), c(0, 1, 2, -1, 4))
  expect_equal(h01[["xmax"]], 2+1/3)
})

test_that("Hulls may be above 0 even if the next point on the curve is negative", {
  skip("Not yet considered")
})
