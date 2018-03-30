library(splitstackshape)
context("concat.split core functions")

test_that("Deprecated functions show warnings", {
  temp <- head(concat.test)
  expect_warning(concat.split.compact(temp, "Likes"))
  expect_warning(concat.split.multiple(
    temp, split.cols = c("Likes", "Hates", "Siblings"),
    seps = c(",", ";", ",")))
})

test_that("cSplit_e returns the correct dim object", {
  DF <- data.frame(V1 = c("1,4,6", "2,6", NA, "", "3,6,6,6"))
  expect_equal(dim(cSplit_e(DF, "V1")), c(5, 7))
  expect_equal(dim(cSplit_e(DF, "V1", type = "character")), c(5, 6))
  expect_equal(dim(cSplit_e(DF, "V1", drop = TRUE)), c(5, 6))
  expect_equal(dim(cSplit_e(data.table::as.data.table(DF), "V1", drop = TRUE)),
               c(5, 6))
})

test_that("cSplit_l returns the correct object", {
  DF <- data.frame(V1 = c("1,4,6", "2,6", NA, "", "3,6,6,6"))
  expect_equal(dim(cSplit_l(DF, "V1")), c(5, 2))
  expect_equal(dim(cSplit_l(DF, "V1", drop = TRUE)), c(5, 1))
  expect_equal(dim(cSplit_l(data.table::as.data.table(DF), "V1", drop = TRUE)), 
               c(5, 1))
  expect_true(is.list(cSplit_l(DF, "V1")$V1_list))
  expect_true(all(sapply(cSplit_l(DF, "V1")$V1_list, is.numeric)))
  DF <- data.frame(V1 = c("1,4,6", "2,6", NA, "A", "3,6,6,6"))
  expect_true(all(sapply(cSplit_l(DF, "V1")$V1_list, is.character)))
})

test_that("concat.split produces relevant messages", {
  temp <- head(concat.test)
  expect_warning(concat.split(temp, "Likes", mode = "binary"))
  expect_warning(concat.split(temp, "Likes", structure = "list", mode = "binary"))
  expect_equal(dim(
    concat.split(temp, "Likes", structure = "expanded", type = "numeric", mode = "binary")),
    c(6, 10)
  )
})

test_that("read.concat works as expected", {
  vec <- c("a,b", "c,d,e", "f, g", "h, i, j,k")
  expect_equal(dim(read.concat(vec, "var", ",")), c(4, 4))
})
