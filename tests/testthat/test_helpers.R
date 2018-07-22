library(splitstackshape)
context("testing helper functions")

test_that("helper functions do what they should do", {
  y <- paste0(1:3, "Var", LETTERS[1:3])
  expect_equal(names(NoSep(y, charfirst = FALSE)), c(".time_1", ".var"))
  DF <- data.frame(c("A", "B"), c("C", "D"))
  expect_true(all(sapply(FacsToChars(DF), is.character)))
  vec <- c("a", "b", "c")
  expect_equal(.collapseMe(vec), "^a|^b|^c")
  expect_equal(.collapseMe(vec, FALSE), "a$|b$|c$")
  vec <- c("", "a", "b")
  expect_equal(.noEmpty(vec), c("a", "b"))
  vec <- c("varA.1", "varA.2", "varB.1")
  expect_equal(vGrep(c("varA", "varB"), vec),
               structure(list(varA = 1:2, varB = 3L), .Names = c("varA", "varB")))
})
