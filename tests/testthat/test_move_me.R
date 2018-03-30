library(splitstackshape)
context("move_me moves things around")

test_that("move_me moves things to the correct place", {
  vec <- letters[1:10]
  expect_equal(move_me(vec, "j first"), c("j", letters[1:9]))
  expect_equal(move_me(vec, "a last"), c(letters[2:10], "a"))
  expect_equal(move_me(vec, "a after c"), c("b", "c", "a", letters[4:10]))
  expect_equal(move_me(vec, "j before e"), letters[c(1:4, 10, 5:9)])
  expect_equal(
    move_me(vec, "a last; b, e, g before d; c first; h after j"),
    c("c", "b", "e", "g", "d", "f", "i", "j", "h", "a"))
})