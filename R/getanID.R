#' Add an "id" Variable to a Dataset
#' 
#' Many functions will not work properly if there are duplicated ID variables in 
#' a dataset. This function is a convenience function for [data.table::rowid()] 
#' and is largely unnecessary now that `rowid` exists.
#' 
#' @param indt The input `data.table` or `data.frame`
#' @param id.vars The variables that should be treated as ID variables. Defaults 
#' to `NULL`, at which point all variables are used to create the new ID variable.
#' @param check Logical. Should the dataset be checked for existing uniqueness
#' within the `id.vars`? Defaults to `TRUE`.
#' @return If `check = TRUE`, the input dataset if ID variables are already unique, or the input 
#' dataset with a new column named "`sub_id`". If `check = FALSE`, the input dataset with a new
#' column named "`sub_id`".
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
getanID <- function(indt, id.vars = NULL, check = TRUE) {
  sub_id <- rowidv(indt, if (is.null(id.vars)) names(indt) else id.vars)
  
  if (isTRUE(check)) {
    if (length(unique(sub_id)) > 1L) cbind(indt, sub_id) else indt
  } else {
    cbind(indt, sub_id)
  }
}
NULL
