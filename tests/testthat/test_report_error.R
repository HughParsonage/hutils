context("Report errors")

test_that("Report error", {
  expect_identical(format_hint(c("(A parenthesis.)",
                                 "Non-parenthesis",
                                 "(A question?)",
                                 "B question?")),
            c("(A parenthesis?)", "(Non-parenthesis?)", "(A question?)", "(B question?)"))
  
  expect_error(report_error(x,
                            "was not the case.",
                            "It must be.", 
                            advice = "It must be!",
                            hint = "Do you believe it?"),
               regexp = "(Do you believe it?)",
               fixed = TRUE)
})
