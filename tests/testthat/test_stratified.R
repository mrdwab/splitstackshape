library(splitstackshape)
context("testing stratified")

test_that("stratified returns the expected number of rows", {
  set.seed(1)
  DF <- data.frame(
    ID = 1:100, A = sample(c("AA", "BB", "CC", "DD", "EE"), 100, replace = TRUE),
    B = rnorm(100), C = abs(round(rnorm(100), digits=1)),
    D = sample(c("CA", "NY", "TX"), 100, replace = TRUE),
    E = sample(c("M", "F"), 100, replace = TRUE))
  expect_equal(nrow(stratified(DF, "A", .1)), 10)
  expect_equal(nrow(stratified(DF, "A", .1, select = list(A = c("AA", "BB")))), 3)
  expect_error(stratified(DF, "A", .1, select = list(c("AA", "BB"))))
  expect_error(stratified(DF, "A", .1, select = list(Ax = c("AA", "BB"))))
  expect_equal(nrow(stratified(DF, group = 5, size = 5)), 15)
  expect_equal(length(stratified(DF, group = 5, size = 5, bothSets = TRUE)), 2)
  expect_message(stratified(DF, c("E", "D", "A"), size = 2))
  expect_error(stratified(data.table::as.data.table(DF), "A", c(1, 3)))
  expect_error(stratified(DF, "A", c(AA = 1, BB = 3, CC = 2, DD = 0, EE = 0)))
  expect_equal(nrow(
    stratified(as.data.table(DF), "A", c(AA = 1, BB = 3, CC = 2), 
               select = list(A = c("AA", "BB", "CC")))), 6)
  expect_error(stratified(as.data.table(DF), "A", c(1, 2, 3, 2, 1)))
  df <- data.frame(x = c(1,1,2,2,2,7), t = 1:6)
  expect_true(all(replicate(10, stratified(df, "x", 1)[x %in% 7, t]) == 6))
})
