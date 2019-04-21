library(splitstackshape)
context("Core cSplit function tests")

test_that("relevant messages are returned", {
  expect_message(cSplit(data.frame(ID = 1:2, V1 = c("A", "B")), "V1"),
                 "Expected more than 1 column. Trying with `t_split`.")
})