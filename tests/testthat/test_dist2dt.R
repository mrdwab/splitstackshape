library(splitstackshape)
context("Distance matrix to data.table")

test_that("correct dim for output", {
  dd <- as.dist((1 - cor(USJudgeRatings)[1:5, 1:5])/2)
  expect_equal(dim(dist2dt(dd)), c(10, 3))
  expect_error(dist2dt(as.matrix(dd)))
})

