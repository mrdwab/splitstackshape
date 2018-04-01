library(splitstackshape)
context("ftables and arrays are converted to data.tables")

test_that("correct dim for output", {
  ftb <- ftable(Titanic, row.vars = 1:3)
  expect_equal(dim(ftable2dt(ftb)), c(16, 5))
  expect_equal(dim(ftable2dt(ftb, "long")), c(32, 5))
  x <- array(1:3, c(2,4,4))
  expect_equal(names(array2dt(x)), c("V1", "V2", "1", "2", "3", "4"))
  expect_equal(dim(array2dt(x)), c(8, 6))
  x <- `dim<-`(1, 1)
  expect_error(ftable2dt(x))
  m <- matrix(1:4, nrow = 2)
  expect_equal(dim(array2dt(m)), c(2, 2))
  expect_equal(dim(array2dt(m, "long")), c(4, 3))
})

