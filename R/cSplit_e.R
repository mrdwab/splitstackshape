#' Split Concatenated Values into their Corresponding Column Position
#' 
#' "Expand" concatenated numeric or character values to their relevant position
#' in a `data.table` or create a binary or count representation of such data.
#' 
#' @param indt The source `data.table`.
#' @param splitCols The variables that need to be split. Can be either the name
#' or the index position.
#' @param sep The character or a vector of characters separating each value. Can
#' also be a regular expression.
#' @param mode Can be either `"binary"` (where the presence of a number in a
#' given column is converted to "1"), `"value"` (where the value is retained and 
#' not recoded to "1"), or `"count"` (where the frequency of each value is
#' is returned). Defaults to `"binary"`.
#' @param type Can be either `"numeric"` (where the items being split are
#' integers) or `"character"` (where the items being split are character 
#' strings). Defaults to `"numeric"`.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' `FALSE`.
#' @param fixed Used for [base::strsplit()] for allowing regular expressions to 
#' be used when splitting the values.
#' @param fill Desired "fill" value. Defaults to `NULL` at which reasonable
#' options are provided by default. If `mode = "binary"` or `mode = "count"`,
#' the default fill would be "0". If `mode = "value"`, the default fill would 
#' be `NA`.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [concat.split()], [concat.split.list()], [num_mat()], [char_mat()]
#' @examples
#' 
#' DT <- data.frame(ID = 1:5, V1 = c("1,2,3", "2,2,2,4", NA, "", "2,4"), 
#'                  V2 = c("A ; B", " D; D; D ", "E", "", NA))
#' cSplit_e(DT, c("V1", "V2"), c(",", ";"), mode = "binary", 
#'          type = c("numeric", "character"))
#' cSplit_e(DT, c("V1", "V2"), c(",", ";"), mode = "count", 
#'          type = c("numeric", "character"))
#' 
#' @aliases cSplit_e
#' @aliases concat.split.expanded
#' @rdname concat.split.expanded
#' @name concat.split.expanded
#' 
#' @export concat.split.expanded
#' @export cSplit_e
cSplit_e <- concat.split.expanded <- function(
  indt, splitCols, sep = ",", mode = "binary", type = "numeric",
  drop = FALSE, fixed = TRUE, fill = NULL) {
  
  indt <- data.table::setDT(copy(indt))
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (length(sep) == 1) sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) stop("Wrong number of sep supplied")
  if (length(mode) == 1) mode <- rep(mode, length(splitCols))
  if (length(mode) != length(mode)) stop("Wrong number of mode supplied")
  if (any(!mode %in% c("binary", "value", "count"))) {
    stop("Mode must be `binary`, `value`, or `count`")
  }
  if (length(type) == 1) type <- rep(type, length(splitCols))
  if (length(type) != length(type)) stop("Wrong number of type supplied")
  
  for (i in seq_along(splitCols)) {
    a <- strsplit(as.character(indt[[splitCols[i]]]), sep[i], fixed = fixed)
    TYPE <- type[i]
    if (TYPE == "character") a <- trim_list(a)
    temp <- switch(
      TYPE,
      numeric = num_mat(a, mode = mode[i], fill = fill),
      character = char_mat(a, mode = mode[i], fill = fill),
      stop("type must be numeric or character"))
    NAMES <- if (typeof(type.convert(colnames(temp), as.is = TRUE)) == "character") {
      sprintf("%s_%s", splitCols[i], colnames(temp))
    } else {
      sprintf("%s_%s", splitCols[i], seq.int(ncol(temp)))
    }
    if (any(duplicated(splitCols))) {
      NAMES <- sprintf("%s_%s", NAMES, mode[i])
    }
    set(indt, j = NAMES, 
        value = as.data.table(temp))
    if (drop) set(indt, j = splitCols[i], value = NULL)
  }
  indt[]
}
NULL
