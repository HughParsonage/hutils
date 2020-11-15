test_that("bearing works", {
  expect_equal(bearing(0, 0, 90, 0), 0)
  expect_equal(bearing(0, 90, 0, 0), 270)
})

test_that("compass2bearing", {
  expect_equal(compass2bearing("N"), 0)
  expect_equal(compass2bearing("NW"), 315)
  expect_equal(compass2bearing("NNW"), 337.5)
})

test_that("easterly_component", {
  expect_equal(easterly_component("NW"), -1/sqrt(2))
  expect_equal(northerly_component("NW"), 1/sqrt(2))
})

