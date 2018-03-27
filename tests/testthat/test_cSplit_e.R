library(splitstackshape)
context("Expanding concatenated columns to count, binary, or value")

test_that("Correct number of rows and columns are returned", {
  DT <- data.frame(ID = 1:5, V1 = c("1,2,3", "2,2,2,4", NA, "", "2,4"),
                   V2 = c("A ; B", " D; D; D ", "E", "", NA))
  expect_equal(dim(cSplit_e(DT, "V1", ",")), c(5, 7))
  expect_equal(dim(cSplit_e(DT, "V1", ",", drop = TRUE)), c(5, 6))
  expect_equal(dim(cSplit_e(DT, c("V1", "V1"), ",", c("binary", "count"))),
               c(5, 11))
  expect_equal(names(cSplit_e(DT, c("V1", "V1"), ",", c("binary", "count"))),
               c("ID", "V1", "V2", "V1_1_binary", "V1_2_binary", "V1_3_binary", 
                 "V1_4_binary", "V1_1_count", "V1_2_count", "V1_3_count", 
                 "V1_4_count"))
  expect_equal(cSplit_e(DT, "V2", ";", "count", "character")$V2_D,
               c(0, 3, 0, 0, 0))
  expect_error(cSplit_e(DT, "V1", ",", "length"))
})