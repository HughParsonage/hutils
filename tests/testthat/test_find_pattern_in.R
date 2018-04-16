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

test_that("File extension", {
  skip_on_cran()
  current_wd <- getwd()
  tempdir <- tempdir()
  skip_if(length(dir(tempdir, pattern = "\\.(zzy|R)$")))
  for (x in LETTERS) {
    writeLines(x, file.path(tempdir, paste0(x, ".zzy")))
  }
  rm(x)
  out1 <- find_pattern_in("A", basedir = tempdir, file.ext = "*zzy")
  out2 <- find_pattern_in("A", basedir = tempdir, file.ext = ".zzy")
  expect_equal(nrow(out1), 1)
  expect_equal(nrow(out2), 1)
  setwd(current_wd)
  
})

test_that("Other file extensions", {
  skip_on_cran()
  skip_if_not(identical(.Platform$OS, "windows"))
  current_wd <- getwd()
  tempdir <- tempdir()
  skip_if(length(dir(tempdir, pattern = "\\.(zfy|R)$")))
  for (x in letters) {
    writeLines(x, file.path(tempdir, paste0(x, ".zfy")))
  }
  rm(x)
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = "zfy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = ".zfy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  expect_warning(out <- find_pattern_in("[yz]",
                                        basedir = tempdir,
                                        use.OS = TRUE,
                                        file.ext = "*.zfy"))
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  out <- find_pattern_in("[yz]",
                         basedir = tempdir,
                         use.OS = FALSE,
                         file.ext = ".zfy")
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out[["line_no"]]), 1)
  
  setwd(current_wd)
})

test_that("Add coverage", {
  skip_on_cran()
  current_wd <- getwd()
  tempdir <- tempdir()
  skip_if(length(dir(tempdir, pattern = "\\.(wfy|R)$")))
  for (x in letters) {
    writeLines(x, file.path(tempdir, paste0(x, ".wfy")))
  }
  rm(x)
  suppressWarnings({
    out <- find_pattern_in("[yz]",
                            basedir = tempdir,
                         use.OS = TRUE,
                         file.ext = "wfy")
  })
  expect_equal(nrow(out), 2L)
  
})

test_that("On Windows", {
  skip_on_cran()
  skip_if_not(identical(.Platform$OS, "windows"))
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


