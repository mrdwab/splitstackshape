library(splitstackshape)
context("concat.split core functions")

test_that("concat.split produces relevant messages", {
  temp <- head(concat.test)
  expect_warning(concat.split(temp, "Likes", mode = "binary"))
  expect_warning(concat.split(temp, "Likes", structure = "list", mode = "binary"))
  expect_equal(dim(
    concat.split(temp, "Likes", structure = "expanded", type = "numeric", mode = "binary")),
    c(6, 10)
  )
})
