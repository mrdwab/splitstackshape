library(splitstackshape)
context("cSplit functions")

test_that("Relevant errors and warnings are produced", {
  temp <- head(concat.test)
  expect_error(cSplit(temp, "Likes", c(",", ";")))
})

test_that("correct directions are used", {
  temp <- head(concat.test)
  expect_equal(dim(cSplit(temp, "Likes", ",", "long", makeEqual = TRUE)),
               c(30, 4))
  expect_equal(dim(cSplit(temp, "Likes", ",", "long")), c(28, 4))
  expect_message(cSplit(temp, "Likes", ",", "wide", makeEqual = FALSE),
                 "makeEqual specified as FALSE but set to TRUE")
})

