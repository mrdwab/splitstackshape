library(splitstackshape)
context("Utility functions work as expected")

test_that("extract_ftable returns correct values", {
  tab <- ftable(Titanic, row.vars = 1:2, col.vars = 3:4)
  expect_true(any(class(tab[c("1st", "3rd"), "Male", "Child" , "Yes" ]) %in% "ftable"))
  expect_equal(c(tab[c("1st", "3rd"), "Male", "Child" , "Yes" ]), c(5, 13))
  expect_equal(c(tab[c("1st", "3rd"), , "Child" , "Yes" ]), c(5, 1, 13, 14))
})

test_that("array_extractor maintains dimensions to extent possible", {
  A1 <- A2 <- array(1:12, dim = c(2, 3, 2))
  dimnames(A1) <- list(c("D_11", "D_12"), c("D_21", "D_22", "D_23"), c("D_31", "D_32"))
  expect_true(is.array(array_extractor(A1, list("D_11", "D_21", "D_31"))))
  expect_equal(c(array_extractor(A1, list("D_11", "D_21", NULL))), c(1, 7))
  expect_error(array_extractor(A2, list("D_11", "D_21", NULL)))
  expect_error(array_extractor(A1, list(1, 3, 2)))
})