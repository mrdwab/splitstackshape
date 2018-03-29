#' Add an "id" Variable to a Dataset
#' 
#' Many functions will not work properly if there are duplicated ID variables in 
#' a dataset. This function is a convenience function for [data.table::rowid()] 
#' and is largely unnecessary now that `rowid` exists.
#' 
#' @param indt The input `data.table`.
#' @param id.vars The variables that should be treated as ID variables. Defaults 
#' to `NULL`, at which point all variables are used to create the new ID variable.
#' @return The input dataset if ID variables are already unique, or the input 
#' dataset with a new column named "`sub_id`".
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(IDA = c("a", "a", "a", "b", "b"),
#'                    IDB = c(1, 1, 1, 1, 1), values = 1:5)
#' mydf
#' getanID(mydf, c("IDA", "IDB"))
#' 
#' mydf <- data.frame(IDA = c("a", "a", "a", "b", "b"),
#'                    IDB = c(1, 2, 1, 1, 2), values = 1:5)
#' mydf
#' getanID(mydf, 1:2)
#' 
#' @export getanID
getanID <- function(indt, id.vars = NULL) {
  indt <- setDT(copy(indt))
  if (is.null(id.vars)) id.vars <- names(indt)
  if (is.numeric(id.vars)) id.vars <- names(indt)[id.vars]
  
  sub_id <- NULL
  
  if (any(duplicated(indt, by = id.vars))) {
    indt[, sub_id := rowidv(.SD), .SDcols = id.vars][]
  } else {
    indt[]
  }
}
NULL
