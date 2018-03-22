library(splitstackshape)
context("Splitting vectors into columns")

test_that("t_split is used as f_split fallback", {
  vec <- c("rockSEPelectro", "rockSEPjazz", NA, "jazzSEPjazzSEPjazz")
  expect_message(f_split(vec, "SEP"), 
                 "Unsupported `sep`. Splitting with `f_split` instead.")
  expect_message(f_split(vec, "[A-Z]+", fixed = FALSE),
                 "Unsupported `sep`. Splitting with `f_split` instead.")
  vec <- c("A;B", "A", "A", "A")
  expect_message(f_split(vec, ";"), 
                 "Expected more than 1 column. Splitting with `t_split`.")
})

test_that("t_split and f_split may have different number of columns", {
  vec <- c("1;2;4;", "1;3;4;", "3;")
  expect_equal(ncol(t_split(vec, ";")), 3)
  expect_equal(ncol(f_split(vec, ";")), 4)
})

test_that("all columns are converted with f_split", {
  vec <- c("1;A", "2;A;2.3", "3;B;1.97")
  expect_equal(sapply(f_split(vec, ";"), class),
               setNames(c("integer", "character", "numeric"), 
                        c("V1", "V2", "V3")))
  expect_equal(sapply(t_split(vec, ";"), class),
               setNames(c("character", "character", "character"), 
                        c("V1", "V2", "V3")))
})

test_that("whitespace is preserved with stripWhite = FALSE", {
  vec <- c("   1;2   ", "3;   4   ")
  expect_equal(nchar(f_split(vec, ";", type.convert = FALSE, 
                             stripWhite = FALSE)$V1), c(4, 1))
  expect_equal(nchar(t_split(vec, ";", stripWhite = FALSE)$V1), c(4, 1))
  expect_message(t_split(vec, ";", stripWhite = FALSE),
                 "type.convert requires stripWhite = TRUE. Setting type.convert = FALSE.")
})