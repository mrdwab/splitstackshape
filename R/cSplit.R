#' Split Concatenated Values in Columns into Separate Columns
#' 
#' The `cSplit` function is designed to quickly and conveniently split
#' concatenated data into separate columns.
#' 
#' @param indt The input `data.table`.
#' @param splitCols The column or columns that have values which need to be split.
#' @param sep The values that serve as a delimiter *within* each column. This 
#' can be a single value if all columns have the same delimiter, or a vector of 
#' values *in the same order as the delimiters in each of the `splitCols`*.
#' @param direction The desired direction of the results, either `"wide"` or `"long"`.
#' @param fixed Logical. Should the split character be treated as a fixed 
#' pattern (`TRUE`) or a regular expression (`FALSE`)? Defaults to `TRUE`.
#' @param drop Logical. Should the original concatenated column be dropped?
#' Defaults to `TRUE`.
#' @param stripWhite Logical. If there is whitespace around the delimiter in
#' the concatenated columns, should it be stripped from the resulting columns? 
#' Defaults to `TRUE`.
#' @param makeEqual Logical. Should all groups be made to be the same length or
#' the same number of columns? Defaults to `FALSE`.
#' @param type.convert Logical. Should [utils::type.convert()] be used to convert
#' the result of each column? This might add a little bit of time to the 
#' execution time.
#' @return A `data.table` with the values split into new columns or rows.
#' @note The `cSplit` function replaces most of the earlier `concat.split*` 
#' functions. The earlier functions remain for compatability purposes, but now 
#' they are essentially wrappers around the `cSplit` function and are likely to 
#' be deprecated over time.
#' @author Ananda Mahto
#' @seealso [concat.split()]
#' @examples
#' 
#' DT <- head(concat.test)
#' 
#' ## Split the "Likes" column
#' cSplit(DT, "Likes")
#' 
#' ## Split the "Likes" and "Hates" columns --
#' ##   they have different delimiters...
#' cSplit(DT, c("Likes", "Hates"), c(",", ";"))
#' 
#' ## Split "Siblings" into a long form...
#' cSplit(DT, "Siblings", ",", direction = "long")
#' 
#' ## Split "Siblings" into a long form, keeping extra whitespace
#' cSplit(DT, "Siblings", ",", direction = "long", stripWhite = FALSE)
#' 
#' ## Split a vector
#' y <- c("a_b_c", "a_b", "c_a_b")
#' cSplit(data.frame(y), "y", "_")
#' 
#' @export cSplit
cSplit <- function(indt, splitCols, sep = ",", direction = "wide", fixed = TRUE, 
                   drop = TRUE, stripWhite = TRUE, makeEqual = NULL, 
                   type.convert = TRUE) {
  
  dropinid1 <- dropinid2 <- patterns <- NULL
  
  o_names <- names(indt)
  indt <- setDT(copy(indt))
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (length(sep) == 1L) sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) stop("Wrong number of sep supplied")
  
  for (i in seq_along(splitCols)) {
    if (fixed) {
      temp <- f_split(indt[[splitCols[i]]], sep[i], fixed, stripWhite, 
                      type.convert, prefix = NULL)
      set(indt, j = sprintf("%s_%d", splitCols[i], seq_along(temp)), value = temp)
      if (drop) set(indt, j = splitCols[i], value = NULL)
    } else {
      temp <- t_split(indt[[splitCols[i]]], sep[i], fixed, stripWhite, 
                      type.convert, prefix = NULL)
      set(indt, j = sprintf("%s_%d", splitCols[i], seq_along(temp)), value = temp)
      if (drop) set(indt, j = splitCols[i], value = NULL)
    }
  }
  
  if (direction == "wide") {
    if (isTRUE(makeEqual)) make_equal(indt, splitCols)[] else indt[]
  } else if (direction == "long") {
    indt[, dropinid1 := .I]
    out <- set(suppressWarnings(
      melt(make_equal(indt, splitCols), measure = patterns(splitCols),
           value.name = splitCols)), j = "variable", value = NULL)
    out[, dropinid2 := seq.int(.N), dropinid1]
    setorderv(out, c("dropinid1", "dropinid2"), na.last = TRUE)
    out <- if (isTRUE(makeEqual)) out else out[!(
      long_fixer(out, splitCols) & dropinid2 != 1L)]
    set(out, j = c("dropinid1", "dropinid2"), value = NULL)
    setcolorder(out, o_names)[]
  }
}
NULL
