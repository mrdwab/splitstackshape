library(splitstackshape)
context("Unexported helper functions")

test_that(".tws works as expected", {
  vec_sw <- c(" A", "  B")
  vec_ew <- c("A ", "B  ")
  vec_both <- c(" A  ", "  B  ")
  res <- c("A", "B")
  expect_equal(.tws(vec_sw), res)
  expect_equal(.tws(vec_ew), res)
  expect_equal(.tws(vec_both), res)
})

test_that(".strflat works as expected", {
  vec <- c("A", "B")
  expect_equal(.strflat(vec), paste(vec, collapse = "\n"))
})

test_that("all_names produces messages when expected", {
  vec <- c("ID1", "ID2", "A_1", "A_2", "B_1")
  expect_message(all_names(vec, c("A_", "B_")), 
                 "All non-stub names being used as ids")
  expect_equal(all_names(vec, c("A_", "B_"), ids = "ID1")$full_names,
               c("ID1", "ID2", "A_1", "A_2", "B_1", "B_2"))
  expect_equal(all_names(vec, c("A_", "B_"), ids = "ID1", 
                         keep_all = FALSE)$full_names,
               c("ID1", "A_1", "A_2", "B_1", "B_2"))
})

test_that("name convenience functons work as expected", {
  mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  expect_equal(othernames(mydf, "a"), setdiff(names(mydf), "a"))
  expect_equal(Names(mydf, c(1, 3)), c("a", "c"))
  expect_equal(Names(mydf, c("a", "c")), Names(mydf, c(1, 3)))
})

test_that("NoSep wors as expected", {
  x <- paste0("Var", LETTERS[1:2], 1:2)
  y <- paste0(1:2, "Var", LETTERS[1:2])
  expect_equal(NoSep(x), data.frame(variable = c("VarA", "VarB"), measure = 1:2, 
                                    stringsAsFactors = FALSE))
  expect_equal(NoSep(y, charfirst = FALSE),
               data.frame(measure = 1:2, variable = c("VarA", "VarB"), 
                          stringsAsFactors = FALSE))
})