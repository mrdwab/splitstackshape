library(splitstackshape)
context("listCol_l, listCol_w, expandRows, getanID work as expected")

test_that("correct dims are returned", {
  dat <- data.frame(A = 1:4, B = I(list(c(1, 2), c(1, 3, 5), c(4), numeric())))
  expect_equal(dim(listCol_l(dat, "B")), c(7, 2))
  expect_equal(dim(listCol_w(dat, "B")), c(4, 4))
  mydf <- data.frame(x = c("a", "b", "q"), 
                     y = c("c", "d", "r"), 
                     count = c(2, 5, 3))
  expect_equal(dim(expandRows(mydf, "count")), c(10, 2))
  expect_equal(dim(expandRows(mydf, 3, count.is.col = FALSE)), c(9, 3))
  expect_message(dim(expandRows(mydf, c(0, 2, 2), count.is.col = FALSE)))
  mydf <- data.frame(IDA = c("a", "a", "a", "b", "b"),
                     IDB = c(1, 1, 1, 1, 1), values = 1:5)
  expect_equal(dim(getanID(mydf, "IDA")), c(5, 4))
  expect_equal(dim(getanID(mydf)), dim(mydf))
})
