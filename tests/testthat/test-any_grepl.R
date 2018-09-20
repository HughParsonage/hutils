context("test-any_grepl")

test_that("Reversed order", {
  expect_equal(any_grepl("a", letters),
               any_grepl(letters, "a"))
  expect_message(any_grepl("a", letters), 
                 regexp = "[Rr]evers")
})
