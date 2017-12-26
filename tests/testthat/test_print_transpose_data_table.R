context("Print transpose")

test_that("Sink to file", {
  Dt <- data.table(ABC = 1:5, 
                   the_a = c("abc", "d", "fg", "XXXX", "\\the"),
                   ef = c(0, 0, NA, -1, 0))
  provide.dir("transpose")
  print_transpose_data.table(Dt, file = "./transpose/tested.txt")
  tested <- readLines("./transpose/tested.txt")
  expected <- readLines("./transpose/expected.txt")
  expect_identical(tested, expected)
  
  print_transpose_data.table(Dt, file = "./transpose/tested.txt", append = TRUE)
  tested_twice <- readLines("./transpose/tested.txt")
  expect_equal(length(tested_twice), length(tested) * 2L, tol = 1L)
  if (file.exists("./transpose/tested.txt")) {
    file.remove("./transpose/tested.txt")
  }
  
})
