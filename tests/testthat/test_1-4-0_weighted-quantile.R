context("weighted_quantile")

test_that("Agreement with Hmisc", {
  skip_if_not_installed("Hmisc")
  x <- rlnorm(50e3, 4)
  w <- sample(500:5000, replace = TRUE, size = 50e3)
  expect_equal(as.vector(Hmisc::wtd.quantile(x, w)),
               as.vector(weighted_quantile(x, w)))
  
  expect_message(weighted_quantile(x),
                 "returning unweighted quantiles")
  expect_equal(weighted_quantile(x),
               as.vector(quantile(x)))
  
  ox <- order(x)
  x <- x[ox]
  w <- w[ox]
  expect_equal(weighted_quantile(x, w),
               weighted_quantile(x, w, v_is_sorted = TRUE))
  
})

test_that("Error handling", {
  expect_error(weighted_quantile(p = NULL),
               "`p` had length zero.",
               fixed = TRUE)
  expect_error(weighted_quantile(p = "a"),
               "`p` was .*but must be numeric")
  expect_error(weighted_quantile(p = c(1, NA_real_)),
               "contained missing values")
  expect_error(weighted_quantile(p = -1),
               "negative values")
  expect_error(weighted_quantile(p = 2),
               "All values in `p` must be in [0, 1].",
               fixed = TRUE)
  expect_error(weighted_quantile(1:5, 1:6),
               regexp = "length.*must be equal")
})

