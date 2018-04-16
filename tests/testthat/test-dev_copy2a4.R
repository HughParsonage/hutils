context("dev_copy2a4")

test_that("Error handling", {
  expect_error(dev_copy2a4(), regexp = "`file` is missing, with no default.", fixed = TRUE)
  expect_error(dev_copy2a4("foo"), regexp = "`file` does not end with .pdf", fixed = TRUE)
})

test_that("Copies to pdf", {
  skip_on_cran()
  skip_if_not_installed("tools")
  tempfile1 <- tempfile(fileext = ".pdf")
  tempfile2 <- tempfile(fileext = ".pdf")
  tempfile3 <- tempfile(fileext = ".pdf")
  tempfile4 <- tempfile(fileext = ".pdf")
  plot(1:10)
  dev.copy2pdf(file = tempfile1, width = 11.69, height = 8.27)
  dev.off()
  plot(1:10)
  dev_copy2a4(file = tempfile3)
  dev.off()
  expect_identical(tools::md5sum(tempfile1),
                   tools::md5sum(tmpefile3))
})

