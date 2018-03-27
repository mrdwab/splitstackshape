library(splitstackshape)
context("Unexported helper functions")

test_that(".tws works as expected", {
  vec_sw <- c(" A", "  B")
  vec_ew <- c("A ", "B  ")
  vec_both <- c(" A  ", "  B  ")
  res <- c("A", "B")
  expect_equal(.tws(vec_sw), res)
  expect_equal(.tws(vec_ew), res)
  expect_equal(.tws(vec_both), res)
})

test_that(".strflat works as expected", {
  vec <- c("A", "B")
  expect_equal(.strflat(vec), paste(vec, collapse = "\n"))
})

test_that("all_names produces messages when expected", {
  vec <- c("ID1", "ID2", "A_1", "A_2", "B_1")
  expect_message(all_names(vec, c("A_", "B_")), 
                 "All non-stub names being used as ids")
})