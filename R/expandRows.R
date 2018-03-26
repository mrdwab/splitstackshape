#' Expand the Rows of a Dataset
#' 
#' Expands (replicates) the rows of a `data.table`, either by a fixed number, a 
#' specified vector, or a value contained in one of the columns in the source 
#' `data.table`.
#'  
#' @param dataset The input `data.table`.
#' @param count The numeric vector of counts *OR* the column from the dataset 
#' that contains the count data. If `count` is a single digit, it is assumed 
#' that all rows should be repeated by this amount.
#' @param count.is.col Logical. Is the `count` value a column from the input
#' dataset? Defaults to `TRUE`.
#' @param drop Logical. If `count.is.col = TRUE`, should the "count" column be 
#' dropped from the result? Defaults to `TRUE`.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(x = c("a", "b", "q"), 
#'                    y = c("c", "d", "r"), 
#'                    count = c(2, 5, 3))
#' mydf
#' expandRows(mydf, "count")
#' expandRows(mydf, count = 3) ## This takes values from the third column!
#' expandRows(mydf, count = 3, count.is.col = FALSE)
#' expandRows(mydf, count = c(1, 5, 9), count.is.col = FALSE)
#' 
#' @export expandRows
expandRows <- function(indt, count, count.is.col = TRUE, drop = TRUE) {
  indt <- setDT(copy(indt))
  
  if (count.is.col) {
    if (is.numeric(count)) count <- names(indt)[count]
    vals <- indt[[count]]
    out <- indt[rep.int(seq.int(nrow(indt)), vals)]
    if (drop) set(out, j = count, value = NULL)
    if (any(vals == 0)) setattr(out, "dropped_vals", indt[vals == 0])
  } else if (length(count) == 1L) {
    vals <- count
    out <- indt[rep(seq.int(nrow(indt)), each = vals)]
  } else {
    vals <- count
    if (length(vals) != nrow(indt)) stop("wrong number of count values supplied")
    out <- indt[rep.int(seq.int(nrow(indt)), as.integer(vals))]
    if (any(vals == 0)) setattr(out, "dropped_vals", indt[vals == 0])
  }
  
  out[]
}
NULL
