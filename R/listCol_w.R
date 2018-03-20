#' Flatten a Column Stored as a List
#' 
#' Flattens a column stored as a \code{list} into a wide form.
#' 
#' @usage listCol_w(inDT, listcol, drop = TRUE, fill = NA_character_)
#' @param inDT The input dataset.
#' @param listcol The name of the column stored as a \code{list}.
#' @param drop Logical. Should the original column be dropped? Defaults to \code{TRUE}.
#' @param fill The desired fill value. Defaults to \code{NA_character_}.
#' @return A \code{data.table}.
#' @author Ananda Mahto
#' @seealso \code{\link{listCol_l}} to unlist a \code{list} column into a "long" format.
#' @examples
#' 
#' dat <- data.frame(A = 1:3, B = I(list(c(1, 2), c(1, 3, 5), c(4))))
#' listCol_w(dat, "B")
#' 
#' @export listCol_w
listCol_w <- function(inDT, listcol, drop = TRUE, fill = NA_character_) {
  if (!is.data.table(inDT)) inDT <- as.data.table(inDT)
  else inDT <- copy(inDT)
  LC <- Names(inDT, listcol)
  reps <- vapply(inDT[[listcol]], length, 1L)
  Nam <- paste(LC, .pad(sequence(max(reps))), sep = "_fl_")
  M <- matrix(fill, nrow = nrow(inDT), ncol = max(reps), 
              dimnames = list(NULL, Nam))
  M[cbind(rep(sequence(nrow(inDT)), reps), sequence(reps))] <- unlist(
    inDT[[listcol]], use.names = FALSE)
  if (isTRUE(drop)) {
    inDT[, (LC) := NULL]
  }
  cbind(inDT, M)
}
NULL