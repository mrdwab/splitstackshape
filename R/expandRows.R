#' "Expand" the rows of a \code{data.frame} or a \code{data.table}.
#' 
#' Expands (replicates) the rows of a \code{data.frame} or a \code{data.table}, either by a fixed
#' number, a specified vector, or a value contained in one of the columns in
#' the source \code{data.frame} or a \code{data.table}.
#' 
#' 
#' @param dataset The input \code{data.frame} or \code{data.table}.
#' @param count The numeric vector of counts OR the column from the
#' dataset that contains the count data. If \code{count} is a single
#' digit, it is assumed that all rows should be repeated by this amount.
#' @param count.is.col Logical. Is the \code{count} value a column from the
#' input dataset? Defaults to \code{TRUE}.
#' @param drop Logical. If \code{count.is.col = TRUE}, should the "count"
#' column be dropped from the result? Defaults to \code{TRUE}.
#' @return A \code{data.frame} or a \code{data.table}, depending on the input.
#' @author Ananda Mahto
#' @references \url{http://stackoverflow.com/a/19519828/1270695}
#' @examples
#' 
#' mydf <- data.frame(x = c("a", "b", "q"), 
#'                    y = c("c", "d", "r"), 
#'                    count = c(2, 5, 3))
#' library(data.table)
#' DT <- as.data.table(mydf)
#' mydf
#' expandRows(mydf, "count")
#' expandRows(DT, "count", drop = FALSE)
#' expandRows(mydf, count = 3)
#' expandRows(mydf, count = 3, count.is.col = FALSE)
#' expandRows(mydf, count = c(1, 5, 9), count.is.col = FALSE)
#' expandRows(DT, count = c(1, 5, 9), count.is.col = FALSE)
#' 
#' @export expandRows
expandRows <- function(dataset, count, count.is.col = TRUE, drop = TRUE) {
  if (!isTRUE(count.is.col)) {
    if (length(count) == 1) {
      dataset[rep(rownames(dataset), each = count), ]
    } else {
      if (length(count) != nrow(dataset)) {
        stop("Expand vector does not match number of rows in data.frame")
      }
      dataset[rep(rownames(dataset), count), ]
    }
  } else {
    if (isTRUE(drop)) {
      dataset[rep(rownames(dataset), dataset[[count]]), 
              setdiff(names(dataset), names(dataset[count]))]
    } else {
      dataset[rep(rownames(dataset), dataset[[count]]), ]
    }
  }
}
NULL