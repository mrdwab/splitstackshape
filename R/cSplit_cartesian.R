#' Split Columns Sequentially to Cartesian Product
#' 
#' @description Splits each requested column one at a time, resulting in the Cartesian
#' Product of the split data.
#' 
#' @param indt The input dataset.
#' @param splitCols The columns to be split.
#' @param sep The separator on which to split each column.
#' @param fixed Logical. Should the split character be treated as a fixed pattern 
#' (`TRUE`) or a regular expression (`FALSE`)? Defaults to `TRUE`.
#' @param stripWhite Logical. Should leading and whitespace be stripped from the
#' output? Defaults to `TRUE`.
#' @param type.convert Logical. Should [utils::type.convert()] be used to convert
#' the result of each column? This might add a little bit of time to the 
#' execution time.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [cSplit()]
#' 
#' @examples
#' DC <- data.frame(AB = c("A", "B"), V1 = c("AB,BW", "x,y,z"), V2 = c("1,2,3", "4,5,6,7"))
#' cartesian_split(DC, c("V1", "V2"))
#' @export 
cartesian_split <- function(indt, splitCols, sep = ",", fixed = TRUE, 
                             stripWhite = TRUE, type.convert = TRUE) {
  indt <- setDT(copy(indt))
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (length(sep) == 1L) sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) stop("Wrong number of sep supplied")
  
  for (i in seq_along(splitCols)) {
    indt <- suppressMessages(
      cSplit(indt, splitCols[i], sep[i], fixed, stripWhite, type.convert, direction = "long"))
  }
  
  indt[]
}
NULL