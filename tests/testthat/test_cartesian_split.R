library(splitstackshape)
context("cartesian_split and cartesian_unlist")

test_that("correct dim for output", {
  DC <- data.frame(AB = c("A", "B"), V1 = c("AB,BW", "x,y,z"), 
                   V2 = c("1,2,3", "4,5,6,7"))
  expect_equal(dim(cartesian_split(DC, c("V1", "V2"))), c(18, 3))
})