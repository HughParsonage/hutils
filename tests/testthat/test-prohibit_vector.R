test_that("prohibit_vector_recycling works", {
  expect_null(prohibit_vector_recycling(1:5, 1:5))
  expect_null(prohibit_vector_recycling(1:5, 1))
  expect_error(prohibit_vector_recycling(1:5, 1:5, 1:6))
  expect_equal(prohibit_vector_recycling.MAXLENGTH(1:5, 1:5), 5L)
  expect_equal(prohibit_vector_recycling.MAXLENGTH(1:5, 1), 5L)
  expect_error(prohibit_vector_recycling.MAXLENGTH(1:5, 1:5, 1:6))
})


test_that("prohibit_length0_vectors", {
  expect_null(prohibit_unequal_length_vectors(1:5, 1:5))
  expect_error(prohibit_unequal_length_vectors(1:5, 1:6))
})

