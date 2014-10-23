#' Unlist a Column Stored as a List
#' 
#' Unlists a column stored as a \code{list} into a long form.
#' 
#' @usage listCol_l(inDT, listcol, drop = TRUE)
#' @param inDT The input dataset.
#' @param listcol The name of the column stored as a \code{list}.
#' @param drop Logical. Should the original column be dropped? Defaults to \code{TRUE}.
#' @return A \code{data.table}.
#' @author Ananda Mahto
#' @seealso \code{\link{listCol_w}} to flatten a \code{list} column into a "wide" format.
#' @examples
#' 
#' dat <- data.frame(A = 1:3, B = I(list(c(1, 2), c(1, 3, 5), c(4))))
#' listCol_l(dat, "B")
#' 
#' @export listCol_l
listCol_l <- function(inDT, listcol, drop = TRUE) {
  if (!is.data.table(inDT)) inDT <- as.data.table(inDT)
  else inDT <- copy(inDT)
  LC <- Names(inDT, listcol)
  reps <- vapply(inDT[[listcol]], length, 1L)
  inDT[[listcol]][reps == 0] <- lapply(inDT[[listcol]][reps == 0], 
                                       function(x) { x <- NA; x})
  reps[reps == 0] <- 1
  temp <- expandRows(inDT, reps, count.is.col = FALSE)
  temp[, ".renameme" := unlist(inDT[[listcol]], use.names = FALSE)]
  setnames(temp, ".renameme", paste0(LC, "_ul"))
  if (isTRUE(drop)) {
    temp[, (LC) := NULL]
  }
  temp
}
NULL
