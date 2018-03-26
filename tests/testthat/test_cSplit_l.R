library(splitstackshape)
context("Splitting concatenated columns into list columns")

test_that("Correct number of rows and columns are returned", {
  DT <- data.frame(ID = 1:5, V1 = c("1,2,3", "2,2,2,4", NA, "", "2,4"), 
                   V2 = c("A ; B", " D; D; D ", "E", "", NA))
  expect_equal(dim(cSplit_l(DT, c("V1", "V2"), c(",", ";"))), c(5, 5))
  expect_equal(names(cSplit_l(DT, c("V1", "V2"), c(",", ";"), drop = TRUE)), 
               names(DT))
  expect_message(
    cSplit_l(DT, "V1", ","), 
    "type.convert requires stripWhite = TRUE. Setting type.convert = FALSE.")
})
