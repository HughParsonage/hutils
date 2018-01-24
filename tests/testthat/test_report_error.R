context("Report errors")

test_that("Report error", {
  expect_identical(format_hint(c("(A parenthesis.)",
                                 "Non-parenthesis",
                                 "(A question?)",
                                 "B question?")),
            c("(A parenthesis?)", "(Non-parenthesis?)", "(A question?)", "(B question?)"))
})
