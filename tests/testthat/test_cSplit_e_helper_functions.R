library(splitstackshape)
context("Trimming vectors and lists")

test_that("Whitespace is trimmed from vectors", {
  expect_equal(trim_vec(c("A ", " B", NA, "")), c("A", "B", NA, ""))
  expect_equal(trim_list(as.list(c("A ", " B", NA, ""))), 
               list("A", "B", NA_character_, NA_character_))
  expect_equal(trim_list(as.list(c("1 ", " 2", NA, "3")), 
                         relist = FALSE, convert = TRUE),
               c(1L, 2L, NA, 3L))
})

context("Creating binary, value, and count matrices")

test_that("Functions return the correct number of columns and rows", {
  invec <- c("1,4,6", "2,6", NA, "3,6,6,6")
  invec <- strsplit(invec, ",")
  expect_equal(ncol(num_mat(invec)), 6)
  expect_equal(nrow(num_mat(invec)), length(invec))
  expect_equal(num_mat(invec, mode = "count")[4, 6], 3)
  invec <- c("rock,electro", "rock,jazz", NA, "jazz,jazz,jazz")
  invec <- strsplit(invec, ",")
  expect_equal(ncol(char_mat(invec)), 3)
  expect_equal(nrow(char_mat(invec)), length(invec))
  expect_equal(char_mat(invec, "count")[4, "jazz"], 3)
})

