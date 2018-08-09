context("any grepl")

test_that("any_grepl", {
  expect_true(any_grepl(c("A_D_E", "K0j"), "[a-z]"))
  expect_true(any_grepl(c("A_D_E", "K0j"), "[A-Z]"))
  expect_false(any_grepl(c("A_D_E", "K0j"), "[A-Z]{2}"))
  expect_false(any_grepl(c("A_D_E", "K0j"), "k.j"))
  expect_false(any_grepl(c("A_D_E", "K0j"), "k.j", fixed = TRUE))
  expect_true(any_grepl(c("A_D_E", "K0j"), "k.j", ignore.case = TRUE))
  expect_true(any_grepl(c("A_D_E", "K0j"), "k0j", ignore.case = TRUE, fixed = TRUE, quiet = TRUE))
  expect_message(any_grepl(c("A_D_E", "K0j"), "k0j", ignore.case = TRUE, fixed = TRUE), 
                 regexp = "so considering both upper and lowercase")
})
