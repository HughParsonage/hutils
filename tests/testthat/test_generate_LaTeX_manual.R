context("LaTeX manual")

test_that("Generate LaTeX manual", {
  skip_on_cran()
  skip_if_not(!file.exists("fastmatch.pdf"))
  generate_LaTeX_manual("fastmatch")
})
