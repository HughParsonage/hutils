context("check_pkg_dependencies")

test_that("Not failing here", {
  skip_on_cran()
  skip_if_not(dir.exists("../../R"))
  expect_null(tryCatch(check_pkg_dependencies(),
                       error = function(e) {
                         check_pkg_dependencies("../..")
                       }))
  
})

