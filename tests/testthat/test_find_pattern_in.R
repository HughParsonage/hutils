context("find_pattern_in")

test_that("Error handling", {
  expect_error(find_pattern_in(warning("unable"), 
                               file.ext = "abc def"),
               regexp = "string of alphanumeric characters")
})

test_that("find_pattern_in", {
  skip_on_cran()
  current_wd <- getwd()
  tempdir <- tempdir()
  skip_if(length(dir(tempdir, pattern = "\\.R$")))
  for (x in letters) {
    writeLines(x, file.path(tempdir, paste0(x, ".R")))
  }
  rm(x)
  
  out <- find_pattern_in("y", basedir = tempdir)
  expect_equal(nrow(out), 1L)
  expect_equal(out[["line_no"]], 1)
  out_null <- find_pattern_in("asifdoh", basedir = tempdir)
  expect_identical(out_null, data.table())
  
  
  setwd(current_wd)
})

test_that("Other file extensions", {
  skip_on_cran()
  current_wd <- getwd()
  tempdir <- tempdir()
  skip_if(length(dir(tempdir, pattern = "\\.(zy|R)$")))
  for (x in letters) {
    writeLines(x, file.path(tempdir, paste0(x, ".zy")))
  }
  rm(x)
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = "zy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = ".zy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = "*.zy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  out <- find_pattern_in("[yz]",
                         basedir = tempdir,
                         use.OS = FALSE,
                         file.ext = ".zy")
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  setwd(current_wd)
})

test_that("On Windows", {
  skip_on_cran()
  current_wd <- getwd()
  temp_dir <- tempfile()
  provide.dir(temp_dir)
  skip_if(length(dir(temp_dir, pattern = "\\.(zy|R)$")))
  for (x in letters) {
    writeLines(x, file.path(temp_dir, paste0(x, ".zy")))
  }
  rm(x)
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = temp_dir,
                                        use.OS = TRUE,
                                        file.ext = "*.zy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
})


