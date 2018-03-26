library(splitstackshape)
context("Expanding the number of rows in a data.table")

test_that("Correct number of rows and columns are returned", {
  mydf <- data.frame(x = c("a", "b", "q"), 
                     y = c("c", "d", "r"), 
                     count = c(2, 5, 3))
  expect_equal(dim(expandRows(mydf, "count")), c(10, 2))
  expect_equal(dim(expandRows(mydf, "count", drop = FALSE)), c(10, 3))
  expect_equal(dim(expandRows(mydf, 3)), c(10, 2))
  expect_equal(dim(expandRows(mydf, 3, count.is.col = FALSE)), c(9, 3))
  expect_equal(dim(expandRows(mydf, c(1, 5, 9), count.is.col = FALSE)), c(15, 3))
})
