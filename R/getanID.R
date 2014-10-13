#' Add an "id" Variable to a Dataset
#' 
#' Many functions will not work properly if there are duplicated ID variables
#' in a dataset. This function is a convenience function for \code{.N} from the "data.table" package to create an "\code{.id}"
#' variable that when used in conjunction with the existing ID variables,
#' should be unique.
#' 
#' 
#' @param data The input \code{data.frame} or \code{data.table}.
#' @param id.vars The variables that should be treated as ID variables. Defaults to \code{NULL}, at which point all variables are used to create the new ID variable.
#' @return The input dataset (as a \code{data.table}) if ID variables are unique, or the input dataset with a new column named "\code{.id}".
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
#' \dontshow{rm(mydf)}
#' 
#' @export getanID
getanID <- function(data, id.vars = NULL) {
  if (!is.data.table(data)) data <- as.data.table(data)
  else data <- copy(data)
  
  if (is.numeric(id.vars)) id.vars <- names(data)[id.vars]
  if (is.null(id.vars)) id.vars <- names(data)
  
  .id <- .N <- NULL
  
  if (any(duplicated(data, by = id.vars))) {
    data[, .id := sequence(.N), by = id.vars]
  } else {
    data
  }
}
