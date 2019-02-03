context("test-dir2")

test_that("Error handling", {
  skip_if_not(identical(.Platform$OS.type, "windows"))
  library(data.table)
  expect_error(dir2(.dont_use = TRUE),
               regexp = "Windows")
  expect_error(dir2(path = raw(1)),
               regexp = "`path` was a raw, but must be a string",
               fixed = TRUE)
  expect_error(dir2(path = data.table(x = 1)),
               regexp = "`path` was a data.table data.frame, but must be a string",
               fixed = TRUE)
  expect_error(dir2(path = character(0)),
               regexp = "`path` was length 0",
               fixed = TRUE)
  theDir <- "a"
  while (dir.exists(theDir)) {
    theDir <- paste0(theDir, "x")
  }
  skip_if(dir.exists(theDir))
  expect_error(dir2(path = theDir),
               regexp = "does not exist")
})

test_that("Error handling (non-Windows)", {
  skip_on_os("windows")
  expect_error(dir2(.dont_use = FALSE),
               regexp = "Windows")
})

test_that("dir2 works", {
  skip_if_not(identical(.Platform$OS.type, "windows"))
  skip_on_cran()
  tempd <- tempdir()
  file.create(file.path(tempd, "abc.csv"))
  file.create(file.path(tempd, "def.csv"))
  z <- dir2(path = tempd, file_ext = ".csv")
  expect_equal(length(z), 2L)
  z <- dir2(path = tempd, file_ext = "*.csv")
  expect_equal(length(z), 2L)
  z1 <- dir2(path = tempd, file_ext = "*.csv", pattern = "^a")
  expect_equal(length(z1), 1L)
})

test_that("Nil files", {
  skip_if_not(identical(.Platform$OS.type, "windows"))
  z <- dir2(file_ext = "*.qqq")
  expect_equal(length(z), 0L)
})
