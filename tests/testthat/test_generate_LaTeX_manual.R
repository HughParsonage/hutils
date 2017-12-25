context("LaTeX manual")

test_that("Generate LaTeX manual", {
  skip_on_cran()
  skip_if_not(!nzchar(Sys.getenv("texi2dvi")))
  skip_if_not(!file.exists("fastmatch.pdf"))
  generate_LaTeX_manual("fastmatch")
  expect_true(file.exists("fastmatch.pdf"))
  if (file.exists("fastmatch.pdf")) {
    file.remove("fastmatch.pdf")
  }
})
