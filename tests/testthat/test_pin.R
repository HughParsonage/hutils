context("%pin%")

test_that("%pin% subset of %in%", {
  sstr <- c("c","ab","B","bba","c",NA,"@","bla","a","Ba","%")
  expect_equal(sstr[sstr %pin% "a"],
               c("ab", "bba", "bla", "a", "Ba"))
  expect_equal(sstr[sstr %pin% c("a", "B")],
               c("ab", "B", "bba", "bla", "a", "Ba"))
})
