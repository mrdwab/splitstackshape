library(splitstackshape)
context("Going from wide to long")

test_that("Correct dimensions of objects are returned", {
  DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"), 
                   varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
                   varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
                   varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))

  expect_equal(dim(reshape_long(DF, c("varA", "varB", "varC"))), c(9, 6))
  expect_equal(names(reshape_long(DF, c("varA", "varB", "varC"), value.name = c("A", "B", "C"))),
               c("id_1", "id_2", "variable", "A", "B", "C"))
  expect_equal(
    dim(reshape_long(DF, c("varA", "varB", "varC"), ids = "id_1", 
                     keep.all = FALSE)), c(9, 5))
  expect_equal(
    dim(reshape_long(DF, c("varA", "varB", "varC"), ids = "id_1", 
                     keep.all = TRUE)), c(9, 6))
  expect_equal(names(reshape_long(DF, c(foo = "varA", bar = "varB", baz = "varC"))),
               c("id_1", "id_2", "variable", "foo", "bar", "baz"))
  expect_equal(dim(merged.stack(DF, var.stubs = c("varA", "varB", "varC"))), c(9, 6))
  expect_equal(
    dim(merged.stack(DF, id.vars = "id_1", var.stubs = c("varA", "varB", "varC"), 
                     keep.all = FALSE)), c(9, 5))

})

test_that("Correct messages are returned", {
  DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"), 
                   varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
                   varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
                   varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
  expect_message(reshape_long(DF, c("varA", "varB", "varC")),
                 "All non-stub names being used as ids")
  expect_message(merged.stack(DF, id.vars = "id_1", var.stubs = "varA"),
                 "This function is deprecated. Use reshape_long instead.")
  expect_warning(merged.stack(DF, id.vars = "id_1", var.stubs = "varA", sep = "."))
})