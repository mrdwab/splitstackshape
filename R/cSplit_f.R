#' Split Concatenated Cells in a data.table
#' 
#' Deprecated. Simply calls [cSplit()], so it is better to use that function 
#' directly rather than continue using this one.
#' 
#' @param indt The input `data.table`.
#' @param splitCols The columns that need to be split up.
#' @param sep The character or characters that serve as delimiters within the 
#' columns that need to be split up. If different columns use different 
#' delimiters, enter the delimiters as a character vector.
#' @param drop Logical. Should the original columns be dropped? Defaults to `TRUE`.
#' @param stripWhite Logical. Should whitespace be removed from character 
#' columns? Defaults to `TRUE`.
#' @param type.convert Logical. Should the output be converted to relevant types
#' or kept as character columns? Defaults to `TRUE`.
#' @return A `data.table`.
#' @author Ananda Mahto. 
#' @examples
#' 
#' ## Sample data. Change `n` to larger values to test on larger data
#' set.seed(1)
#' n <- 10
#' mydf <- data.frame(id = sequence(n))
#' mydf <- within(mydf, {
#'   v3 <- do.call(paste, c(data.frame(matrix(sample(
#'   letters, n*4, TRUE), ncol = 4)), sep = "_"))
#'   v2 <- do.call(paste, c(data.frame(matrix(sample(
#'   LETTERS, n*3, TRUE), ncol = 3)), sep = "."))
#'   v1 <- do.call(paste, c(data.frame(matrix(sample(
#'   10, n*2, TRUE), ncol = 2)), sep = "-"))
#' })
#' mydf
#' 
#' cSplit_f(mydf, splitCols = c("v1", "v2", "v3"), sep = c("-", ".", "_"))
#' 
#' @export cSplit_f
cSplit_f <- function(indt, splitCols, sep, drop = TRUE, stripWhite = TRUE, 
                     type.convert) {
  message("This function is deprecated. Use `cSplit` instead.")
  cSplit(indt, splitCols, sep, direction = "wide", fixed = TRUE, drop, 
         stripWhite, makeEqual = NULL, type.convert)
}
NULL
