context("timing dir")

test_that("Checks timings directory structure when PR", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("TRAVIS"), "true"))
  skip_if(identical(PR <- Sys.getenv("TRAVIS_PULL_REQUEST"), "false"))
  expect_true(PR %in% list.dirs("timings"))
  
})