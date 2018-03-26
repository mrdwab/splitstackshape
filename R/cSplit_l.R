#' Split Concatenated Cells into a List Format
#' 
#' Takes a column in a `data.table` with multiple values, splits the values into 
#' a `list`, and returns a new `data.table`.
#' 
#' @param indt The source `data.table`.
#' @param splitCols The variables that need to be split. Can be either the name
#' or the index position.
#' @param sep The character or a vector of characters separating each value. Can 
#' also be a regular expression.
#' @param fixed Used for [base::strsplit()] for allowing regular expressions to 
#' be used when splitting the values.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' `FALSE`.
#' @param stripWhite Logical. Should leading and trailing whitespace be trimmed
#' from the list items? Defaults to `FALSE`.
#' @param type.convert Logical. Should [utils::type.convert()] be used on the 
#' output? Defaults to `TRUE`.
#' @return A `data.table` with the concatenated column split and added as
#' a `list`. If `drop = FALSE`, the list-columns would be named in the form of
#' `"original_name_list"`. If `drop = TRUE`, the list-columns maintain the 
#' original column names.
#' @author Ananda Mahto
#' @seealso [concat.split()], [concat.split.expanded()]
#' @examples
#' 
#' DT <- data.frame(ID = 1:5, V1 = c("1,2,3", "2,2,2,4", NA, "", "2,4"), 
#'                  V2 = c("A ; B", " D; D; D ", "E", "", NA))
#' cSplit_l(DT, c("V1", "V2"), c(",", ";"))
#' 
#' @aliases cSplit_l
#' @aliases concat.split.list
#' @rdname concat.split.list
#' @name concat.split.list
#' 
#' @export concat.split.list
#' @export cSplit_l
cSplit_l <- concat.split.list <- function(
  indt, splitCols, sep = ",", fixed = TRUE, drop = FALSE, 
  stripWhite = FALSE, type.convert = TRUE, ...) {
  
  indt <- setDT(copy(indt))
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (length(sep) == 1L) sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) stop("Wrong number of sep supplied")
  
  for (i in seq_along(splitCols)) {
    a <- strsplit(as.character(indt[[splitCols[i]]]), sep[i], fixed, ...)
    if (!stripWhite & type.convert) {
      message("type.convert requires stripWhite = TRUE. Setting type.convert = FALSE.")
    }
    if (stripWhite | type.convert) a <- trim_list(a, convert = type.convert)
    if (drop) {
      set(indt, j = splitCols[i], value = a)
    } else {
      set(indt, j = sprintf("%s_list", splitCols[i]), value = a)
    }
  }  
  
  indt[]
}
NULL
