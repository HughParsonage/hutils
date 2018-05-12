context("dev_copy2a4")

test_that("Error handling", {
  expect_error(dev_copy2a4(), regexp = "`filename` is missing, with no default.", fixed = TRUE)
  expect_error(dev_copy2a4("foo"), regexp = '`filename = "foo"` does not end with .pdf', fixed = TRUE)
  expect_error(dev_copy2a4(1:2), regexp = "`filename` had length 2. `filename` must be a length-one character vector.", fixed = TRUE)
  expect_error(dev_copy2a4(1), regexp = "`filename` was type double. `filename = 1` must be a length-one character vector.", fixed = TRUE)
})

test_that("Copies to pdf", {
  skip_on_cran()
  skip_if_not_installed("tools")
  skip_if(identical(Sys.getenv("HUTILS_BENCHMARK"), "TRUE"))
  tempfile1 <- tempfile(fileext = ".pdf")
  tempfile2 <- tempfile(fileext = ".pdf")
  plot(1:10)
  skip_if(grDevices::dev.cur() <= 1)
  tryCatch(dev.copy2pdf(file = tempfile1, width = 11.69, height = 8.27),
           error = function(e) {
             skip(e$m)
           })
  dev.off()
  plot(1:10)
  dev_copy2a4(file = tempfile2)
  dev.off()
  expect_equal(tools::md5sum(tempfile1),
               tools::md5sum(tempfile2),
               check.attributes = FALSE)
})

