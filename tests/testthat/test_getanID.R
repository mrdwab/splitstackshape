library(splitstackshape)
context("Making sure there are unique id variables")

test_that("Correct number of columns are returned", {
  DF <- data.frame(var1 = c("a", "a", "a", "b", "b"),
                   var2 = c(1, 2, 1, 1, 2), var3 = 1:5)
  expect_equal(getanID(DF, "var1")$sub_id, c(1L, 2L, 3L, 1L, 2L))
  expect_equal(getanID(DF, "var1"), getanID(DF, 1))
  expect_equal(getanID(DF, c("var1", "var2"))$sub_id, c(1L, 1L, 2L, 1L, 1L))
  expect_equal(ncol(getanID(DF)), ncol(DF))
})
