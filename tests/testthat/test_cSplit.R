library(splitstackshape)
context("cSplit functions")

test_that("Relevant errors and warnings are produced", {
  temp <- head(concat.test)
  expect_error(cSplit(temp, "Likes", c(",", ";")))
})

test_that("correct directions are used", {
  temp <- head(concat.test)
  expect_equal(dim(cSplit(temp, "Likes", ",", "long", makeEqual = TRUE)),
               c(30, 4))
  expect_equal(dim(cSplit(temp, "Likes", ",", "long")), c(28, 4))
  expect_message(cSplit(temp, "Likes", ",", "wide", makeEqual = FALSE),
                 "makeEqual specified as FALSE but set to TRUE")
})

test_that("cSplit_f works as expected", {
  mydf <- data.frame(
    id = 1:3,
    v1 = c("1 - 2", " 3 - 4 ", "5-6"),
    v2 = c("a.b.c", "d.e.f", "g.h.i"),
    v3 = c("A_B_C_D", "E_F_G_H", "I_J_K_L")
  )
  expect_warning(
    cSplit_f(mydf, splitCols = c("v1", "v2", "v3"), sep = c("-", ".", "_"))
  )
  expect_equal(dim(
    suppressWarnings(cSplit_f(mydf, splitCols = c("v1", "v2", "v3"), 
             sep = c("-", ".", "_"), stripWhite = TRUE))
  ), c(3, 10))
  expect_error(suppressWarnings(cSplit_f(mydf, "v1", c(",", "."))))
})

