library(splitstackshape)
context("testing stratified")

test_that("stratified returns the expected number of rows", {
  set.seed(1)
  dat1 <- data.frame(ID = 1:100,
                     A = sample(c("AA", "BB", "CC", "DD", "EE"), 
                                100, replace = TRUE),
                     B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
                     D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
                     E = sample(c("M", "F"), 100, replace = TRUE))
  expect_equal(nrow(stratified(dat1, "A", .1)), 10)
  expect_equal(nrow(stratified(dat1, "A", .1, select = list(A = c("AA", "BB")))), 3)
  expect_error(stratified(dat1, "A", .1, select = list(c("AA", "BB"))))
  expect_error(stratified(dat1, "A", .1, select = list(Ax = c("AA", "BB"))))
  expect_equal(nrow(stratified(dat1, group = 5, size = 5)), 15)
  expect_equal(length(stratified(dat1, group = 5, size = 5, bothSets = TRUE)), 2)
  expect_message(stratified(dat1, c("E", "D", "A"), size = 2))
  expect_error(stratified(data.table::as.data.table(dat1), "A", c(1, 3)))
  expect_equal(nrow(stratified(dat1, "A", c(AA = 1, BB = 3, CC = 2, DD = 0, EE = 0))), 6)
})
