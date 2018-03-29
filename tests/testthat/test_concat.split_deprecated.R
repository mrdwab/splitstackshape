library(splitstackshape)
context("Making sure deprecated functions show messages and work")

test_that("Correct number of columns are returned for concat.split.compact", {
  DF <- data.frame(ID = 1:3, num_col = c("1,2,6", "3,5", "2,4,7"),
                   char_col = c("  A, B  ", NA, ""),
                   diff_delim_col = c("6;7", "5", "1;3;2"))
  expect_message(concat.split.compact(DF, "num_col", ","),
                 "This function is deprecated. Use `cSplit` instead.")
  expect_equal(ncol(concat.split.compact(DF, "num_col")), 7)
})

test_that("Correct number of columns are returned for concat.split.multiple", {
  DF <- data.frame(ID = 1:3, num_col = c("1,2,6", "3,5", "2,4,7"),
                   char_col = c("  A, B  ", NA, ""),
                   diff_delim_col = c("6;7", "5", "1;3;2"))
  expect_message(concat.split.multiple(DF, "num_col", ","),
                 "This function is deprecated. Use `cSplit` instead.")
  expect_equal(dim(concat.split.multiple(DF, names(DF)[-1], c(",", ",", ";"))),
               c(3, 9))
  expect_equal(dim(concat.split.multiple(DF, names(DF)[-1], c(",", ",", ";"), "long")),
               c(8, 4))
})
