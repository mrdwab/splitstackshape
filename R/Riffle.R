#' Interleaves Values Within Matrices or Vectors
#' 
#' Mimics some of the behavior of the `Riffle` function
#' (<http://reference.wolfram.com/mathematica/ref/Riffle.html>) in Mathematica. 
#' For matrices, it interleaves the columns. For vectors, it interleaves 
#' differently according to whether the subsequent values are presented as 
#' separate values or whether they are grouped with `c()`.
#' 
#' It is expected that all matrices to be interleaved would have the same
#' number of rows, though they may have differing numbers of columns. If they
#' have differing numbers of columns, they are all made to conform to the same
#' dimension before proceeding by recycling the existing columns.
#' 
#' @param \dots The objects or values that need to be interleaved.
#' @return A vector or a matrix depending on the input. If one or more input
#' objects is a matrix, the result will also be a matrix.
#' @author Ananda Mahto
#' @references <http://stackoverflow.com/q/21347207/1270695>
#' @examples
#' 
#' m1 <- matrix(1:9, nrow = 3, ncol = 3)
#' m2 <- matrix(letters[1:9], nrow = 3, ncol = 3)
#' 
#' Riffle(m1, m2)
#' Riffle(m1, "||", m2)
#' 
#' m3 <- matrix(LETTERS[1:6], nrow = 3, ncol = 2)
#' 
#' Riffle(m1, m2, m3)
#' 
#' ## Just vectors
#' 
#' Riffle(1:6, "x")
#' Riffle(1:6, "x", "y")
#' Riffle(1:6, c("x", "y"))
#' 
#' @export
Riffle <- function(...) {
  x <- list(...)
  if (!all(vapply(x, function(y) is.matrix(y) | (is.vector(y) & is.atomic(y)), logical(1L)))) {
    stop("input must be either vectors or matrices")
  }
  isMat <- vapply(x, is.matrix, logical(1L))  
  isVec <- vapply(x, is.vector, logical(1L))
  if (!any(isVec)) LenV <- 0 else LenV <- max(vapply(x[isVec], length, 1L))
  if (!any(isMat)) LenM <- NRow <- LenV else LenM <- max(vapply(x[isMat], length, 1L))
  if (LenV > LenM) stop("longest vector is longer than biggest matrix")
  if (any(isMat)) {
    Dims <- vapply(x[isMat], dim, c(row = 1L, col = 1L))
    if (length(unique(Dims["row", ])) > 1L) {
      stop("All matrices must have the same number of rows")
    }
    MCol <- max(Dims["col", ])
    NRow <- Dims["row", 1L]
  } 
  if (all(isMat)) TYPE <- "allmat"
  if (all(isVec)) TYPE <- "allvec"
  if (sum(isMat) >= 1L & sum(isVec) >= 1L) {
    x[isVec] <- lapply(x[isVec], function(y) {
      matrix(rep(y, length.out = NRow), nrow = NRow, ncol = MCol)
    })
    TYPE <- "allmat"
  }
  switch(
    TYPE, 
    allmat = {
      if (length(unique(Dims["col", ])) > 1) {
        Fix <- which(Dims["col", ] < MCol)
        x[Fix] <- lapply(x[Fix], function(y) {
          matrix(rep(y, length.out = LenM), nrow = Dims["row", 1])
        })
      }
      NewDims <- vapply(x, dim, c(row = 1L, col = 1L))
      A <- do.call(cbind, x)[, order(sequence(rep(NewDims["col", 1], ncol(NewDims))))]
    },
    allvec = {
      x <- lapply(x, function(y) rep(y, length.out = LenV))
      A <- as.vector(t(do.call(cbind, x)))
    })  
  A
}
NULL