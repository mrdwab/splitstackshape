library(splitstackshape)
context("reshape-related functions")

test_that("correct dims and output produced", {
  DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"), 
                   varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
                   varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
                   varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
  expect_equal(
    Reshape(DF, id.vars = c("id_1", "id_2"), 
            var.stubs = c("varA", "varB", "varC"))[time == 3, varC],
    DF$varC.3)
  expect_warning(Reshape(DF, id.vars = c("id_1", "id_2"), 
                         var.stubs = c("varA", "varB", "varC"), 
                         rm.rownames = TRUE))
  expect_equal(
    sapply(Stacked(DF, var.stubs = c("varA", "varB", "varC"), sep = "."), dim),
    sapply(Stacked(data.table::as.data.table(DF), id.vars = 1:2, 
                   var.stubs = c("varA", "varB", "varC"), sep = "."), dim)
  )
  expect_equal(
    dim(merged.stack(DF, var.stubs = c("varA", "varB", "varC"), sep = ".")),
    c(9, 6))
  
  names(DF) <- gsub("(var.*)\\.(\\d+)", "\\1.1.\\2", names(DF))
  expect_equal(
    dim(merged.stack(DF, id.vars = c("id_1", "id_2"), 
                     var.stubs = c("varA", "varB", "varC"), sep = ".")),
    c(9, 7)
  )
  
  DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"), 
                   varA1 = c("a", "b", "c"), varA2 = c("x", "y", "z"), 
                   varB1 = c("q", "r", "s"), varB2 = c(10, 20, 30),
                   varC1 = c(11, 22, 33), varC2 = c(-1.23, .992, -.351))
  expect_equal(dim(
    Reshape(DF, id.vars = c("id_1", "id_2"), 
            var.stubs = c("varA", "varB", "varC"), sep = "NoSep")
  ), c(6, 6))
  expect_equal(dim(
    Reshape(data.table::as.data.table(DF), id.vars = c("id_1", "id_2"), 
            var.stubs = c("varA", "varB", "varC"), sep = "NoSep")
  ), c(6, 6))
})
