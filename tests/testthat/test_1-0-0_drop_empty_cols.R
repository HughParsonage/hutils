context("Empty columns")

test_that("Drops empty columns of data.tables and data.frames", {
  library(data.table)
  DT1 <- data.table(x1 = 1:5,
                    x2 = sample(c(letters[1:5], NA), size = 5), 
                    x3 = runif(5),
                    x4 = sample(c(rcauchy(4), NA)),
                    x5 = c(TRUE, FALSE, TRUE, NA, TRUE),
                    x6 = rep(NA_character_, 5),
                    x7 = rep(NA_integer_, 5))
  
  DF1 <- as.data.frame(DT1)
  
  output <- drop_empty_cols(DT1, copy = TRUE)
  expect_false(any(c("x6", "x7") %in% names(output)))
  expect_true(all(c("x6", "x7") %in% names(DT1)))
  
  output_no_copy <- drop_empty_cols(DT1)
  expect_false(any(c("x6", "x7") %in% names(output_no_copy)))
  expect_false(any(c("x6", "x7") %in% names(DT1)))
  
  output_DF <- drop_empty_cols(DF1)
  expect_false(any(c("x6", "x7") %in% names(output_DF)))
  expect_true(all(c("x6", "x7") %in% names(DF1)))
  
})

test_that("Error handling", {
  expect_error(drop_empty_cols("x"),
               regexp = "must be a data\\.(frame|table)")
  
  expect_warning(drop_empty_cols(data.frame(x = c(1, 2)),
                                 copy = FALSE),
                 regexp = "copy.*is FALSE, but .* not a data\\.table.")
})

test_that("No empty cols", {
  DFi <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_identical(drop_empty_cols(DFi), DFi)
})
