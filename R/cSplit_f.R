#' Split Concatenated Cells in a data.frame or a data.table
#' 
#' A variation of the [concat.split()] family of functions that uses
#' [data.table::fread()] for speedy splitting of concatenated columns of data.
#' 
#' @param indt The input `data.frame` or `data.table`.
#' @param splitCols The columns that need to be split up.
#' @param sep The character or characters that serve as delimiters within the 
#' columns that need to be split up. If different columns use different 
#' delimiters, enter the delimiters as a character vector.
#' @param drop Logical. Should the original columns be dropped? Defaults to
#' `TRUE`.
#' @param dotsub The character that should be substituted as a delimiter *if 
#' `sep = "."`. `fread` does not seem to always work nicely with `sep = "."`, so 
#' it needs to be substituted. By default, this function will substitute `"."` 
#' with `"|"`.
#' @param stripWhite Logical. Should whitespace be stripped before writing to 
#' the temporary file? Defaults to `FALSE`.
#' @return A `data.table`.
#' @author Ananda Mahto. Thanks also to Arun Srinivasan for helping to refine 
#' this function.
#' @references <http://stackoverflow.com/a/19231054/1270695>
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
cSplit_f <- function(indt, splitCols, sep, drop = TRUE, dotsub = "|", stripWhite = FALSE) {
  .Deprecated(
    "cSplit", 
    msg = "V2 of splitstackshape will feature a rewritten cSplit that makes this function obsolete.")
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  
  if (!is.data.table(indt)) indt <- as.data.table(indt) 
  else indt <- copy(indt)
  
  if (length(sep) == 1) 
    sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) {
    stop("Verify you have entered the correct number of sep")
  }
  for (i in seq_along(splitCols)) {
    if (sep[i] == ".") {
      indt[, (splitCols[i]) := gsub(
        ".", dotsub, get(splitCols[i]), fixed = TRUE)]
      sep[i] <- dotsub
    }
    x <- tempfile()
    
    if (isTRUE(stripWhite)) {
      writeLines(.stripWhite(indt[[splitCols[i]]], sep[i]), x)
    } else if (!is.character(indt[[splitCols[i]]])) {
      writeLines(as.character(indt[[splitCols[i]]]), x)
    } else {
      writeLines(indt[[splitCols[i]]], x)
    }

    Split <- fread(x, sep[i], header = FALSE)
    split_names <- paste(splitCols[i], seq_along(Split), sep = "_")
    set(indt, i = NULL, j = split_names, value = Split)
  }
  if (isTRUE(drop)) set(indt, i = NULL, j = splitCols, value = NULL)
  indt[]
}
NULL
