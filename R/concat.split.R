#' Split Concatenated Cells into a Condensed Format
#' 
#' The default splitting method for `concat.split`. This is simply a wrapper for
#' [cSplit()].
#' 
#' @param data The source `data.table`.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' `FALSE`.
#' @param fixed Logical. Should the split character be treated as a fixed
#' pattern (`TRUE`) or a regular expression (`FALSE`)? Defaults to `TRUE`.
#' @param \dots optional arguments to pass to [cSplit()].
#' @return A `data.table`.
#' @note This function no longer does anything different from [cSplit()]. It is 
#' recommended that you transition your code to the `cSplit` function instead.
#' @author Ananda Mahto
#' @seealso [cSplit()]
#' @examples
#' 
#' DT <- head(concat.test)
#' concat.split.compact(DT, "Likes")
#' concat.split.compact(DT, 4, ";")
#' 
#' ## Extra arguments to cSplit
#' concat.split.compact(DT, "Siblings", drop = TRUE, stripWhite = TRUE)
#' 
#' @export concat.split.compact
concat.split.compact <- function(data, split.col, sep = ",", 
                                 drop = FALSE, fixed = TRUE, ...) {
  message("This function is deprecated. Use `cSplit` instead.")
  cSplit(indt = data, splitCols = split.col, sep = sep, 
         drop = drop, fixed = fixed, direction = "wide", ...)
}
NULL

#' Split Concatenated Cells in a Dataset
#' 
#' The `concat.split` function takes columns with multiple values, splits the 
#' values into a `list`, into separate columns, or other specified form, and 
#' returns a new `data.table`.
#' 
#' *structure* 
#' 
#' * `"compact"` creates as many columns as the maximum length of the resulting 
#' split. This is the most useful general-case application of this function. 
#' * When the input is numeric, `"expanded"` creates as many columns as the 
#' maximum value of the input data. This is most useful when converting to 
#' `mode = "binary"`. 
#' * `"list"` creates a single new column that is structurally a `list` within a 
#' `data.table`.
#' 
#' *fixed*
#' 
#' * When `structure = "expanded"` or `structure = "list"`, it is possible to 
#' supply a a regular expression containing the characters to split on. For 
#' example, to split on `","`, `";"`, or `"|"`, you can set `sep = ",|;|\|"` or 
#' `sep = "[,;|]"`, and `fixed = FALSE` to split on any of those characters.
#' 
#' @param data The source `data.table`.
#' @param split.col The variable that needs to be split; can be specified
#' either by the column number or the variable name.
#' @param sep The character separating each value (defaults to `","`.
#' @param structure Can be either `"compact"`, `"expanded"`, or `"list"`. 
#' Defaults to `"compact"`. See Details.
#' @param mode Can be either `"binary"`, `"value"`, or `"count"` (where `"binary"` 
#' is default and it recodes values to 1 or 0). This setting only applies when 
#' `structure = "expanded"`; a warning message will be issued if used with other
#' structures.
#' @param type Can be either `"numeric"` or `"character"` (where `"numeric"` is 
#' default). This setting only applies when `structure = "expanded"`; a warning 
#' message will be issued if used with other structures.
#' @param drop Logical. Should the original variable be removed in the output?
#' Defaults to `FALSE`.
#' @param fixed Is the input for the `sep` value `fixed`, or a *regular 
#' expression*? See Details.
#' @param fill The "fill" value for missing values when `structure = "expanded"`. 
#' Defaults to `NA`.
#' @param \dots Additional arguments to [cSplit()].
#' @note This is more of a "legacy" or "convenience" wrapper function 
#' encompassing the features available in the separated functions of [cSplit()],
#' [cSplit_e()], and [cSplit_l()].
#' @author Ananda Mahto
#' @examples
#' 
#' ## Load some data
#' DT <- head(concat.test)
#' 
#' # Split up the second column, selecting by column number
#' concat.split(DT, 2)
#' 
#' # ... or by name, and drop the offensive first column
#' concat.split(DT, "Likes", drop = TRUE)
#' 
#' # The "Hates" column uses a different separator
#' concat.split(DT, "Hates", sep = ";", drop = TRUE)
#' 
#' \dontrun{
#' # You'll get a warning here, when trying to retain the original values
#' concat.split(DT, 2, mode = "value", drop = TRUE)
#' }
#' 
#' # Try again. Notice the differing number of resulting columns
#' concat.split(DT, 2, structure = "expanded", mode = "value", 
#'              type = "numeric", drop = TRUE)
#' 
#' # Let's try splitting some strings... Same syntax
#' concat.split(DT, 3, drop = TRUE)
#' 
#' # Strings can also be split to binary representations
#' concat.split(DT, 3, structure = "expanded", mode = "binary",
#'              type = "character", fill = 0, drop = TRUE)
#' 
#' # Split up the "Likes column" into a list variable; retain original column
#' head(concat.split(DT, 2, structure = "list", drop = FALSE))
#' 
#' # View the structure of the output to verify that the new column is a list; 
#' # note the difference between "Likes" and "Likes_list".
#' str(concat.split(DT, 2, structure = "list", drop = FALSE))
#' 
#' @export concat.split
concat.split <- function(data, split.col, sep = ",", structure = "compact",
                         mode = NULL, type = NULL, drop = FALSE, fixed = FALSE, 
                         fill = NA, ...) {
  
  M1 <- paste(c("", "'mode' supplied but ignored.", 
                "'mode' setting only applicable when structure = 'expanded'"), 
              collapse = "\n")
  M2 <- paste(c("", "'type' supplied but ignored.", 
                "'type' setting only applicable when structure = 'expanded'"), 
              collapse = "\n")
  
  temp <- switch(
    structure, 
    compact = {
      if (!is.null(mode)) warning(M1)
      if (!is.null(type)) warning(M2)
      concat.split.compact(data = data, split.col = split.col, 
                           sep = sep, drop = drop, fixed = fixed)
    },
    list = {
      if (!is.null(mode)) warning(M1)
      if (!is.null(type)) warning(M2)
      concat.split.list(indt = data, splitCols = split.col, 
                        sep = sep, drop = drop, fixed = fixed, ...)
    },
    expanded = {
      concat.split.expanded(indt = data, splitCols = split.col, 
                            sep = sep, mode = mode, type = type, 
                            drop = drop, fixed = fixed, fill = fill)
    },
    stop("'structure' must be either 'compact', 'expanded', or 'list'"))
  temp
}
NULL

#' Split Concatenated Cells and Optionally Reshape the Output
#' 
#' This is a wrapper around the [cSplit()] function to maintain backwards 
#' compatability with earlier versions of the "splitstackshape" package. It 
#' allows the user to split multiple columns at once and optionally convert the 
#' results into a "long" format.
#' 
#' @param data The source `data.table`.
#' @param split.cols A vector of columns that need to be split.
#' @param seps A vector of the separator character used in each column. If all
#' columns use the same character, you can enter that single character.
#' @param direction The desired form of the resulting `data.table`, either 
#' `"wide"` or `"long"`.  Defaults to `"wide"`.
#' @param \dots Other arguments to [cSplit()].
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [cSplit()], [cSplit_e()], [cSplit_l()].
#' @examples
#' 
#' DT <- head(concat.test)
#' concat.split.multiple(DT, split.cols = c("Likes", "Hates", "Siblings"),
#'                       DT = c(",", ";", ","))
#' concat.split.multiple(DT, split.cols = c("Likes", "Siblings"),
#'                       DT = ",", direction = "long")
#' 
#' @export concat.split.multiple
concat.split.multiple <- function(data, split.cols, seps = ",", 
                                  direction = "wide", ...) {
  message("This function is deprecated. Use `cSplit` instead.")
  cSplit(indt = data, splitCols = split.cols, 
         sep = seps, direction = direction, ...)
}
NULL
