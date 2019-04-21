library(splitstackshape)
context("Riffle interleaves things correctly")

test_that("expected output is returned", {
  m1 <- matrix(1:9, nrow = 3, ncol = 3)
  m2 <- matrix(letters[1:9], nrow = 3, ncol = 3)
  m3 <- matrix(LETTERS[1:6], nrow = 3, ncol = 2)
  expect_equal(dim(Riffle(m1, m2)), c(3, 6))
  expect_equal(dim(Riffle(m1, m2, m3)), c(3, 9))
  # Recycling of the first column of m3...
  expect_equal(Riffle(m1, m2, m3)[, 9], m3[, 1])
  expect_equal(dim(Riffle(m1, "||", m2)), c(3, 9))
  expect_equal(Riffle(1:4, "x"), c("1", "x", "2", "x", "3", "x", "4", "x"))
  expect_equal(
    Riffle(1:4, "x", "y"), 
    c("1", "x", "y", "2", "x", "y", "3", "x", "y", "4", "x", "y"))
  expect_equal(Riffle(1:4, c("x", "y")), c("1", "x", "2", "y", "3", "x", "4", "y"))
  expect_error(Riffle(list(1), c(2)))
  expect_error(Riffle(rbind(m1, 1:3), m2))
})

