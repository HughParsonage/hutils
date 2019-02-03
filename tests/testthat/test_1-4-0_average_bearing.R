context("Average bearing")

test_that("average_bearing works", {
  expect_equal(average_bearing(0, 90), 45)
  expect_equal(average_bearing(0, 90), 45)
  expect_equal(average_bearing(0, 180, average_of_opposite = 30), 30)
  expect_equal(average_bearing(0, 180, average_of_opposite = "right"), 90)
  expect_equal(average_bearing(0, 180, average_of_opposite = "left"), 270)
  expect_equal(average_bearing_n(1:179), 90)
})



