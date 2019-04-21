library(splitstackshape)
context("Stacking columns into lists")

test_that("Correct type and dimensions of objects are returned", {
  DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"), 
                   varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
                   varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
                   varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
  expect_message(Stacked(DF, var.stubs = c("varA", "varB", "varC")),
                 "All non-stub names being used as ids")
  expect_true(!is.data.frame(Stacked(DF, var.stubs = c("varA", "varB", "varC"))))
  expect_true(is.data.frame(Stacked(DF, var.stubs = "varA", keep.all = FALSE)))
  expect_equal(sapply(Stacked(DF, var.stubs = c("varA", "varB", "varC")), dim),
               structure(c(9L, 4L, 6L, 4L, 3L, 4L), .Dim = 2:3, 
                         .Dimnames = list(NULL, c("varA", "varB", "varC"))))
})