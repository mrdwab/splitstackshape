library(splitstackshape)
context("Trimming vectors and lists")

test_that("Whitespace is trimmed from vectors", {
  expect_equal(trim_vec(c("A ", " B", NA, "")), c("A", "B", NA, ""))
  expect_equal(trim_list(as.list(c("A ", " B", NA, ""))), 
               list("A", "B", NA_character_, NA_character_))
  expect_equal(trim_list(as.list(c("1 ", " 2", NA, "3")), 
                         relist = FALSE, convert = TRUE),
               c(1L, 2L, NA, 3L))
  expect_error(trim_vec(list(c(1, 2), c(1, 3, 4))))
  expect_equal(names(trim_list(list(a = " 1", b = 2))), c("a", "b"))
  expect_equal(names(trim_list(list(1, 2))), NULL)
})

context("Creating binary, value, and count matrices")

test_that("Functions return the correct number of columns and rows", {
  invec <- c("1,4,6", "2,6", NA, "", "3,6,6,6")
  invec <- strsplit(invec, ",")
  expect_equal(ncol(num_mat(invec)), 6)
  expect_equal(nrow(num_mat(invec)), length(invec))
  expect_equal(num_mat(invec, mode = "count")[5, 6], 3)
  invec <- c("rock,electro", "rock,jazz", NA, "", "jazz,jazz,jazz")
  invec <- strsplit(invec, ",")
  expect_equal(ncol(char_mat(invec)), 3)
  expect_equal(nrow(char_mat(invec)), length(invec))
  expect_equal(char_mat(invec, "count")[5, "jazz"], 3)
})

test_that("fill values work as expected", {
  invec <- c("1,4,6", "2,6", NA, "", "3,6,6,6")
  invec <- strsplit(invec, ",")
  expect_equal(sum(is.na(num_mat(invec, mode = "count", fill = NA))), 23)
  expect_equal(sum(num_mat(invec, mode = "count", fill = 999) == 999), 23)
  expect_error(num_mat(invec, mode = "binary", fill = "X"))
  expect_equal(sum(num_mat(invec, mode = "value", fill = 999) == 999), 23)
  invec <- c("rock,electro", "rock,jazz", NA, "", "jazz,jazz,jazz")
  invec <- strsplit(invec, ",")
  expect_equal(sum(is.na(char_mat(invec, mode = "count", fill = NA))), 10)
  expect_equal(sum(char_mat(invec, mode = "count", fill = 999) == 999), 10)
  expect_error(char_mat(invec, mode = "binary", fill = "X"))
  expect_equal(sum(char_mat(invec, mode = "value", fill = "X") == "X"), 10)
})

test_that("t_split gets called when fixed=FALSE", {
  DF <- data.frame(ID = 1:3, num_col = c("1,2,6", "3,5", "2,4,7"),
                   char_col = c("  A, B  ", NA, ""),
                   diff_delim_col = c("6;7", "5", "1;3;2"))
  expect_equal(dim(cSplit(DF, names(DF)[-1], "[,;]", fixed = FALSE)), c(3, 9))
  
  df <- data.frame(V1 = c("A", "B", "C"),
                   V2 = c("C", "C", NA),
                   V3 = c("D", "A", "B"))
  expect_equal(dim(char_mat(df)), c(3, 4))
})