library(splitstackshape)
context("Unlisting or flattening list columns in a data.table")

test_that("Correct number of rows and columns are returned", {
  dat <- data.frame(A = 1:3, B = I(list(1:2, 1:3, 4)), C = I(list(1, 1:2, 1:4)))
  expect_equal(ncol(unlist_cols(dat, c("B", "C"), direction = "wide", 
                                makeEqual = FALSE)), 
               sum(max(lengths(dat$B)), max(lengths(dat$C))) + 1)
  expect_equal(nrow(unlist_cols(dat, c("B", "C"), direction = "long", 
                                makeEqual = FALSE)), 
               sum(pmax(lengths(dat$B), lengths(dat$C))))
  expect_equal(ncol(unlist_cols(dat, c("B", "C"), direction = "wide", 
                                makeEqual = TRUE)), 
               sum(max(max(lengths(dat$B)), max(lengths(dat$C))) * 2) + 1)
  expect_equal(nrow(unlist_cols(dat, c("B", "C"), direction = "long", 
                                makeEqual = TRUE)), 
               sum(max(pmax(lengths(dat$B), lengths(dat$C))) * nrow(dat)))
  expect_equal(unlist_cols(dat, "B"), listCol_w(dat, "B"))
  expect_equal(unlist_cols(dat, "B", "long"), listCol_l(dat, "B"))
})
