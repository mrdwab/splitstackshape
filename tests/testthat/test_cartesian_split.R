library(splitstackshape)
context("cartesian_split and cartesian_unlist")

test_that("correct dim for output", {
  DC <- data.frame(AB = c("A", "B"), V1 = c("AB,BW", "x,y,z"), 
                   V2 = c("1,2,3", "4,5,6,7"))
  expect_equal(dim(cartesian_split(DC, c("V1", "V2"))), c(18, 3))
  
  L2 <- list(X1 = list("A", c("A", "B"), "X", NULL),
             X2 = list(NULL, c(1, 2, 3), c(1, 2), c(1, 2, 3, 4)),
             X3 = list(c("a", "b"), "c", "d", c("e", "f", "g")))
  expect_equal(dim(cartesian_unlist(L2)), c(22, 4))
})

