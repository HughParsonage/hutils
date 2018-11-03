context("check_pkg_dependencies")

test_that("Not failing here", {
  expect_null(check_pkg_dependencies())
  expect_null(check_pkg_dependencies("../.."))
  expect_true(check_pkg_dependencies(on_success = TRUE))
})

