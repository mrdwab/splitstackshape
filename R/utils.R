replace_empty_arguments <- function(a) {
  empty_symbols <- vapply(a, function(x) is.symbol(x) && identical("", as.character(x)), 0)
  a[!!empty_symbols] <- 0
  lapply(a, eval)
}
NULL

#' @name extract_ftable
#' @title Extract Parts of an `ftable`.
#' 
#' @description Extension of `?Extract` to work with [stats::ftable()] by using
#' the names of the columns and rows (stored in the attributes of the `ftable`).
#' 
#' @param inftable The input `ftable`.
#' @param \dots The values you would like to extract, as character strings for
#' each dimension.
#' 
#' @note The extraction should be specified positionally from left to right for 
#' the row attributes and then top to bottom for the column attributes. For 
#' example, the `tab2` table in the examples has four dimensions: `Class`, `Sex`, 
#' `Age`, and `Survived`, so the approach to extract should be in the form of
#' `tab2[values from "Class", values from "Sex", values from "Age", values from
#' "Survived"]`. So, if you wanted to extract the count of children who did not
#' survive for all classes and genders, you would use `tab2[, , "Child", "No"]`.
#' 
#' @examples
#' mytable <- ftable(Titanic, row.vars = 1:3)
#' mytable[c("1st", "3rd"), , , "No"]
#' tab2 <- ftable(Titanic, row.vars = 1:2, col.vars = 3:4)
#' tab2[c("1st", "3rd"), , , ]
#' @export 
`[.ftable` <- function (inftable, ...) {
  tblatr <- attributes(inftable)[c("row.vars", "col.vars")]
  valslist <- replace_empty_arguments(as.list(match.call()[-(1:2)]))
  x <- sapply(valslist, function(x) identical(x, 0))
  TAB <- as.table(inftable)
  valslist[x] <- dimnames(TAB)[x]
  temp <- as.matrix(expand.grid(valslist))
  out <- ftable(
    `dimnames<-`(`dim<-`(TAB[temp], lengths(valslist)), valslist),
    row.vars = seq_along(tblatr[["row.vars"]]),
    col.vars = seq_along(tblatr[["col.vars"]]) + length(tblatr[["row.vars"]]))
  names(attributes(out)[["row.vars"]]) <- names(tblatr[["row.vars"]])
  names(attributes(out)[["col.vars"]]) <- names(tblatr[["col.vars"]])
  out
}
NULL

#' Extract Parts of an `array` Using `dimnames`
#' 
#' Uses a `list` of the same length as the dimensions of an array to extract
#' information from an array, similar to using matrix indexing.
#' 
#' @param inarray The input array.
#' @param valslist A list of vectors to use to extract data. A value of `NULL`
#' for any of the list elements will return all values for that dimension.
#' @return An array.
#' @author Ananda Mahto
#' @references <http://stackoverflow.com/q/34795331/1270695>
#' @note The `list` used for `valslist` must be the same length as the number of 
#' dimensions in the array. It must also be specified in the same order as you 
#' would normally reference the dimensions of an array. For instance, in the 
#' example, the array has (in this order) row dimensions, column dimensions, and 
#' a third dimension. 
#' @examples
#' 
#' my_array <- structure(1:12, .Dim = c(2L, 3L, 2L), 
#'   .Dimnames = list(c("D_11", "D_12"), 
#'   c("D_21", "D_22", "D_23"), c("D_31", "D_32")))
#' my_array
#' 
#' array_extractor(my_array, list("D_11", NULL, NULL))
#' array_extractor(my_array, list(NULL, "D_21", "D_32"))
#' array_extractor(my_array, list(NULL, c("D_21", "D_22"), NULL))
#' 
#' @export
array_extractor <- function(inarray, valslist) {
  if (any(!unlist(valslist) %in% unlist(dimnames(inarray)))) {
    stop("valslist not all found in dimnames")
  }
  x <- vapply(valslist, is.null, logical(1L))
  valslist[x] <- dimnames(inarray)[x]
  temp <- as.matrix(expand.grid(valslist))
  `dimnames<-`(`dim<-`(inarray[temp], lengths(valslist)), valslist)
}
NULL