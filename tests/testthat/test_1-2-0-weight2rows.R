context("weight2rows")

test_that("Non-numeric var", {
  expect_error(weight2rows(data.frame(x = 1:10,
                                      y = letters[1:5],
                                      stringsAsFactors = FALSE),
                           "y"), 
               regexp = "Non-numeric weight.var",
               fixed = TRUE)
})