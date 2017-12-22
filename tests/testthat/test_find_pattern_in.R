context("find_pattern_in")

test_that("find_pattern_in", {
  skip_on_cran()
  current_wd <- getwd()
  tempdir <- tempdir()
  for (x in letters) {
    writeLines(x, file.path(tempdir, paste0(x, ".R")))
  }
  rm(x)
  
  out <- find_pattern_in("y", basedir = tempdir)
  expect_equal(nrow(out), 1L)
  expect_equal(out[["line_no"]], 1)
  
  
  
  setwd(current_wd)
})
