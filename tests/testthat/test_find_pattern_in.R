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

test_that("Multiples", {
  tempd <- tempfile()
  skip_if(dir.exists(tempd))
  dir.create(tempd)
  tempf <- tempfile(fileext = ".R", tmpdir = tempd)
  writeLines(c("afpdom", "afp", "# afp"), 
             tempf)
  out <- find_pattern_in("afp", tempd, which_lines = "all")
  expect_equal(nrow(out), 2)
})


test_that("include comments", {
  skip_on_cran()
  tempd <- tempfile()
  skip_if(dir.exists(tempd))
  dir.create(tempd)
  temp.R <- tempfile(fileext = ".R", tmpdir = tempd)
  writeLines(c("# This is an R script that doesn't contain fooc", 
               "x <- 1", 
               ""),
             temp.R)
  expect_equal(nrow(find_pattern_in("fooc", tempd)), 0)
  
  temp2.R <- tempfile(fileext = ".R", tmpdir = tempd)
  writeLines(c("# This is an R script that does contain fooc", 
               "fooc <- 1", 
               ""),
             temp2.R)
  expect_equal(nrow(find_pattern_in("fooc", tempd)), 1)
  
  temp3.tex <- tempfile(fileext = ".tex", tmpdir = tempd)
  writeLines(c("% File not containing fooc", 
               "\\begin{document}", 
               "food", 
               "\\end{document}", 
               ""), 
             temp3.tex)
  expect_equal(nrow(find_pattern_in("fooc", tempd, file_pattern = "(R|tex)$")), 1)
  expect_equal(nrow(find_pattern_in("fooc",
                                    tempd,
                                    include.comments = TRUE,
                                    file_pattern = "(R|tex)$")),
               3)
  expect_equal(nrow(find_pattern_in("fooc",
                                    tempd,
                                    include.comments = FALSE,
                                    comment.char = "%", # i.e. keep R comments
                                    file_pattern = "(R|tex)$")),
               2)
  
  
  
})

test_that("ignore_case", {
  temp_dir <- tempfile("ignore-case")
  provide.dir(temp_dir)
  writeLines(c("qwerty", "Qwerty"), file.path(temp_dir, "a.R"))
  oic <- find_pattern_in("qwerty",
                         temp_dir, 
                         file_contents_ignore_case = TRUE,
                         which = "all")
  expect_equal(nrow(oic), 2)
})

test_that("perl+fixed", {
  temp_dir <- tempfile("ignore-case")
  provide.dir(temp_dir)
  writeLines(c("qwerty", "Qwerty"), file.path(temp_dir, "a.R"))
  # now warning
  oic <- find_pattern_in("qwerty",
                         temp_dir, 
                         file_contents_fixed = TRUE,
                         which = "all")
  expect_equal(nrow(oic), 1)
})

test_that("swapped arguments", {
  expect_warning(find_pattern_in(".", "x.*y"))
})


