#' Add an "id" variable to a dataset
#' 
#' Many functions will not work properly if there are duplicated ID variables
#' in a dataset. This function is a convenience function for using
#' \code{\link{ave}} and \code{\link{seq_along}} to create an \code{.id}
#' variable that when used in conjunction with the existing ID variables,
#' should be unique.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param id.vars The variables that should be treated as ID variables.
#' @return The input \code{data.frame} if ID variables are unique or the input
#' \code{data.frame} with a new \code{.id} column.
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
getanID <- function(data, id.vars) {
  x <- do.call(paste0, data[id.vars])
  if (any(duplicated(x))) {
    data[[".id"]] <- ave(x, x, FUN = seq_along)
    data
  } else {
    data
  }
}
