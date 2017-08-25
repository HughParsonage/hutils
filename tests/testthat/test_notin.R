context("%notin%")

test_that("Complement of %in%", {
  x <- sample(1:10e3, size = 10)
  y <- sample(1:10e3, size = 10)
  expect_true(all(xor(x %in% y, 
                      x %notin% y)))
  
  x <- sample(letters, size = 10)
  y <- sample(letters, size = 10)
  expect_true(all(xor(x %in% y, 
                      x %notin% y)))
})

test_that("y NULL", {
  expect_true(all(5 %notin% NULL))
})

test_that("DTs", {
  List <- 
    list("ABC" = 2,
         list("elements" = data.frame(xx = 1,
                                      yy = 2,
                                      zz = 3)))
  expect_false("xx" %notin% names(List[[2]]$elements))
  expect_true("ww" %notin% names(List[[2]]$elements))
  
})

