test_that("provide.file works", {
  skip_on_cran()
  tempf <- tempfile()
  expect_equal(provide.file(tempf), tempf)
  # Again for file existence
  expect_equal(provide.file(tempf), tempf)
  expect_equal(provide.file(tempf, on_failure = stop("failed")), tempf)
  
})

test_that("provide.file failure", {
  skip_on_cran()
  skip_on_os(os = c("mac", "linux", "solaris"))
  # Should fail
  expect_equal(provide.file("////"), "")
  expect_equal(provide.file("////", TRUE), TRUE)
  expect_error(provide.file("////", on_failure = stop("ossif09")),
               regexp = "ossif09")
})

