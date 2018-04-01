#' Expand the Rows of a Dataset
#' 
#' Expands (replicates) the rows of a `data.table` or a `data.frame`, either by 
#' a fixed number, a specified vector, or a value contained in one of the 
#' columns in the source `data.table`.
#'  
#' @param data The input dataset.
#' @param count The numeric vector of counts *OR* the column from the dataset 
#' that contains the count data. If `count` is a single digit, it is assumed 
#' that all rows should be repeated by this amount.
#' @param count.is.col Logical. Is the `count` value a column from the input
#' dataset? Defaults to `TRUE`.
#' @param drop Logical. If `count.is.col = TRUE`, should the "count" column be 
#' dropped from the result? Defaults to `TRUE`.
#' @return A `data.table` or a `data.frame`, depending on the input.
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(x = c("a", "b", "q"), y = c("c", "d", "r"), count = c(2, 5, 3))
#' expandRows(mydf, "count")
#' expandRows(mydf, count = 3) ## This takes values from the third column!
#' expandRows(mydf, count = 3, count.is.col = FALSE)
#' expandRows(mydf, count = c(1, 5, 9), count.is.col = FALSE)
#' 
#' ## Separate method for `data.table`s
#' expandRows(data.table::as.data.table(mydf), "count")
#' 
#' @export expandRows
expandRows <- function(data, count, count.is.col = TRUE, drop = TRUE){
  UseMethod("expandRows")
}
NULL

#' @export
expandRows.data.table <- function(data, count, count.is.col = TRUE, drop = TRUE) {
  if (count.is.col) {
    if (is.numeric(count)) count <- names(data)[count]
    vals <- data[[count]]
    out <- data[rep.int(seq.int(nrow(data)), vals)]
    if (drop) set(out, j = count, value = NULL)
    if (any(vals == 0)) setattr(out, "dropped_vals", data[vals == 0])
  } else if (length(count) == 1L) {
    vals <- count
    out <- data[rep(seq.int(nrow(data)), each = vals)]
  } else {
    vals <- count
    if (length(vals) != nrow(data)) stop("wrong number of count values supplied")
    out <- data[rep.int(seq.int(nrow(data)), as.integer(vals))]
    if (any(vals == 0)) setattr(out, "dropped_vals", data[vals == 0])
  }
  
  out[]
}
NULL

#' @export
expandRows.data.frame <- function(data, count, count.is.col = TRUE, drop = TRUE) {
  if (count.is.col) {
    if (is.numeric(count)) count <- names(data)[count]
    vals <- data[[count]]
    out <- data[rep.int(seq.int(nrow(data)), vals), ]
    if (drop) out[[count]] <- NULL
    if (any(vals == 0)) setattr(out, "dropped_vals", data[vals == 0, ])
  } else if (length(count) == 1L) {
    vals <- count
    out <- data[rep(seq.int(nrow(data)), each = vals), ]
  } else {
    vals <- count
    if (length(vals) != nrow(data)) stop("wrong number of count values supplied")
    out <- data[rep.int(seq.int(nrow(data)), as.integer(vals)), ]
    if (any(vals == 0)) setattr(out, "dropped_vals", data[vals == 0])
  }
  
  `rownames<-`(out, NULL)
}
NULL
