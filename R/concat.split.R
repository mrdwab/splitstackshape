#' Split Concatenated Cells into a Condensed Format
#' 
#' The default splitting method for \code{\link{concat.split}}. Formerly based
#' on \code{\link{read.concat}} but presently a simple wrapper for
#' \code{\link{cSplit}}.
#' 
#' 
#' @param data The source \code{data.frame} or \code{data.table}
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Logical. Should the split character be treated as a fixed
#' pattern (\code{TRUE}) or a regular expression (\code{FALSE})? Defaults to
#' \code{TRUE}.
#' @param \dots optional arguments to pass to \code{cSplit}.
#' @return A \code{data.table}.
#' @note This function no longer does anything different from
#' \code{\link{cSplit}}. It is recommended that you transition your code to the
#' \code{cSplit} function instead.
#' @author Ananda Mahto
#' @seealso \code{\link{read.concat}}, \code{\link{cSplit}}
#' @examples
#' 
#' temp <- head(concat.test)
#' concat.split.compact(temp, "Likes")
#' concat.split.compact(temp, 4, ";")
#' 
#' ## Extra arguments to cSplit
#' concat.split.compact(temp, "Siblings", drop = TRUE, stripWhite = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @export concat.split.compact
concat.split.compact <- function(data, split.col, sep = ",", 
                                 drop = FALSE, fixed = TRUE, ...) {
  message("This function is deprecated. Use `cSplit` instead.")
  cSplit(indt = data, splitCols = split.col, sep = sep, 
         drop = drop, fixed = fixed, direction = "wide", ...)
}
NULL





#' Split Concatenated Values into their Corresponding Column Position
#' 
#' "Expand" concatenated numeric or character values to their relevant position
#' in a \code{data.frame} or \code{data.table} or create a binary representation of such data.
#' 
#' 
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param mode Can be either \code{"binary"} (where presence of a number in a
#' given column is converted to "1") or \code{"value"} (where the value is
#' retained and not recoded to "1"). Defaults to \code{"binary"}.
#' @param type Can be either \code{"numeric"} (where the items being split are
#' integers) or \code{"character"} (where the items being split are character
#' strings). Defaults to \code{"numeric"}.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{strsplit} for allowing regular expressions to be
#' used.
#' @param fill Desired "fill" value. Defaults to \code{NA}.
#' @return A \code{data.frame} or a \code{data.table} depending on the source input.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.list}},
#' \code{\link{concat.split.compact}}, \code{\link{concat.split.multiple}},
#' \code{\link{numMat}}, \code{\link{charMat}}
#' @examples
#' 
#' temp <- head(concat.test)
#' cSplit_e(temp, "Likes")
#' cSplit_e(temp, 4, ";", fill = 0)
#'  
#' ## The old function name still works
#' concat.split.expanded(temp, "Likes")
#' concat.split.expanded(temp, 4, ";", fill = 0)
#' concat.split.expanded(temp, 4, ";", mode = "value", drop = TRUE)
#' concat.split.expanded(temp, "Siblings", type = "character", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @aliases cSplit_e
#' @aliases concat.split.expanded
#' @rdname concat.split.expanded
#' @name concat.split.expanded
#' 
#' @export concat.split.expanded
#' @export cSplit_e
cSplit_e <- concat.split.expanded <- function(data, split.col, sep = ",", mode = NULL, 
                                  type = "numeric", drop = FALSE, 
                                  fixed = TRUE, fill = NA) {
  if (is.numeric(split.col)) split.col <- Names(data, split.col)
  if (!is.character(data[[split.col]])) a <- as.character(data[[split.col]])
  else a <- data[[split.col]]
  if (is.null(mode)) mode = "binary"  
  b <- strsplit(a, sep, fixed = fixed)
  b <- lapply(b, trim)
  
  temp1 <- switch(
    type,
    character = {
      temp1 <- charMat(b, fill = fill, mode = mode)
      colnames(temp1) <- paste(split.col, colnames(temp1), sep = "_")
      temp1
    },
    numeric = {
      nchars <- max(nchar(unlist(b, use.names = FALSE)))
      temp1 <- numMat(b, fill = fill, mode = mode)
      colnames(temp1) <- paste(split.col, .pad(seq_len(ncol(temp1))), sep = "_")
      temp1
    },
    stop("'type' must be either 'character' or 'numeric'"))
  if (isTRUE(drop)) {
    if (is.data.table(data)) {
      cbind(data, temp1)[, (split.col) := NULL][]
    } else {
      cbind(data[othernames(data, split.col)], temp1)
    }
  } 
  else cbind(data, temp1)
}
NULL







#' Split Concatenated Cells into a List Format
#' 
#' Takes a column in a \code{data.frame} or \code{data.table} with multiple values, splits the
#' values into a \code{list}, and returns a new \code{data.frame} or \code{data.table}.
#' 
#' 
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{\link{strsplit}} for allowing regular
#' expressions to be used.
#' @return A \code{data.frame} or \code{data.table} with the concatenated column split and added as
#' a \code{list}.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.multiple}}
#' @examples
#' 
#' temp <- head(concat.test)
#' str(cSplit_l(temp, "Likes"))
#' cSplit_l(temp, 4, ";")
#' 
#' ## The old function name still works
#' str(concat.split.list(temp, "Likes"))
#' concat.split.list(temp, 4, ";")
#' concat.split.list(temp, 4, ";", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @aliases cSplit_l
#' @aliases concat.split.list
#' @rdname concat.split.list
#' @name concat.split.list
#' 
#' @export concat.split.list
#' @export cSplit_l
cSplit_l <- concat.split.list <- function(data, split.col, sep = ",", 
                              drop = FALSE, fixed = FALSE) {
  if (is.numeric(split.col)) split.col <- Names(data, split.col)
  a <- .stripWhite(data[[split.col]], sep)
  
  varname <- paste(split.col, "list", sep="_")
  b <- strsplit(a, sep, fixed = fixed)
  
  if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
    data[[varname]] <- I(b)
  } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
    data[[varname]] <- I(lapply(b, as.numeric))
  }
  if (isTRUE(drop)) {
    if (is.data.table(data)) {
      data[, (split.col) := NULL][]
    } else {
      data[othernames(data, split.col)]
    }
  } 
  else data
}
NULL



















#' Split Concatenated Cells in a Dataset
#' 
#' The \code{concat.split} function takes a column with multiple values, splits
#' the values into a \code{list} or into separate columns, and returns a new
#' \code{data.frame} or \code{data.table}.
#' 
#' \emph{structure} \itemize{ \item \code{"compact"} creates as many columns as
#' the maximum length of the resulting split. This is the most useful
#' general-case application of this function. \item When the input is numeric,
#' \code{"expanded"} creates as many columns as the maximum value of the input
#' data. This is most useful when converting to \code{mode = "binary"}. \item
#' \code{"list"} creates a single new column that is structurally a
#' \code{\link{list}} within a \code{\link{data.frame}} or \code{data.table}. }
#' \emph{fixed} \itemize{ \item When \code{structure = "expanded"} or
#' \code{structure = "list"}, it is possible to supply a a regular expression
#' containing the characters to split on.  For example, to split on \code{","},
#' \code{";"}, or \code{"|"}, you can set \code{sep = ",|;|\|"} or \code{sep =
#' "[,;|]"}, and \code{fixed = FALSE} to split on any of those characters.}
#' 
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.col The variable that needs to be split; can be specified
#' either by the column number or the variable name.
#' @param sep The character separating each value (defaults to \code{","}).
#' @param structure Can be either \code{"compact"}, \code{"expanded"}, or
#' \code{"list"}. Defaults to \code{"compact"}. See Details.
#' @param mode Can be either \code{"binary"} or \code{"value"} (where
#' \code{"binary"} is default and it recodes values to 1 or \code{NA}, like
#' Boolean data, but without assuming 0 when data is not available).  This
#' setting only applies when \code{structure = "expanded"}; a warning message
#' will be issued if used with other structures.
#' @param type Can be either \code{"numeric"} or \code{"character"} (where
#' \code{"numeric"} is default).  This setting only applies when
#' \code{structure = "expanded"}; a warning message will be issued if used with
#' other structures.
#' @param drop Logical (whether to remove the original variable from the output
#' or not). Defaults to \code{FALSE}.
#' @param fixed Is the input for the \code{sep} value \emph{fixed}, or a
#' \emph{regular expression}? See Details.
#' @param fill The "fill" value for missing values when \code{structure =
#' "expanded"}. Defaults to \code{NA}.
#' @param \dots Additional arguments to \code{\link{cSplit}}.
#' @note This is more of a "legacy" or "convenience" wrapper function
#' encompassing the features available in the separated functions of
#' \code{\link{cSplit}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.list}}, and \code{\link{concat.split.expanded}}.
#' @author Ananda Mahto
#' @seealso \code{\link{cSplit}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.list}},
#' \code{\link{concat.split.multiple}}
#' @examples
#' 
#' ## Load some data
#' temp <- head(concat.test)
#' 
#' # Split up the second column, selecting by column number
#' concat.split(temp, 2)
#' 
#' # ... or by name, and drop the offensive first column
#' concat.split(temp, "Likes", drop = TRUE)
#' 
#' # The "Hates" column uses a different separator
#' concat.split(temp, "Hates", sep = ";", drop = TRUE)
#' 
#' \dontrun{
#' # You'll get a warning here, when trying to retain the original values
#' concat.split(temp, 2, mode = "value", drop = TRUE)
#' }
#' 
#' # Try again. Notice the differing number of resulting columns
#' concat.split(temp, 2, structure = "expanded",
#' mode = "value", type = "numeric", drop = TRUE)
#' 
#' # Let's try splitting some strings... Same syntax
#' concat.split(temp, 3, drop = TRUE)
#' 
#' # Strings can also be split to binary representations
#' concat.split(temp, 3, structure = "expanded",
#' type = "character", fill = 0, drop = TRUE)
#' 
#' # Split up the "Likes column" into a list variable; retain original column
#' head(concat.split(concat.test, 2, structure = "list", drop = FALSE))
#' 
#' # View the structure of the output to verify
#' # that the new column is a list; note the
#' # difference between "Likes" and "Likes_list".
#' str(concat.split(temp, 2, structure = "list", drop = FALSE))
#' 
#' @export concat.split
concat.split <- function(data, split.col, sep = ",", structure = "compact",
                         mode = NULL, type = NULL, drop = FALSE, fixed = FALSE, 
                         fill = NA, ...) {
  
  M1 <- paste(c("", "'mode' supplied but ignored.", 
                "'mode' setting only applicable",
                "when structure = 'expanded'"), collapse = "\n")
  M2 <- paste(c("", "'type' supplied but ignored.", 
                "'type' setting only applicable",
                "when structure = 'expanded'"), collapse = "\n")
  
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
      concat.split.list(data = data, split.col = split.col, 
                        sep = sep, drop = drop, fixed = fixed)
    },
    expanded = {
      concat.split.expanded(data = data, split.col = split.col, 
                            sep = sep, mode = mode, type = type, 
                            drop = drop, fixed = fixed, fill = fill)
    },
    stop("'structure' must be either 'compact', 'expanded', or 'list'"))
  temp
}
NULL



















#' Split Concatenated Cells and Optionally Reshape the Output
#' 
#' This is a wrapper for the \code{\link{cSplit}} function to maintain
#' backwards compatability with earlier versions of the "splitstackshape"
#' package. It allows the user to split multiple columns at once and optionally
#' convert the results into a "long" format.
#' 
#' 
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.cols A vector of columns that need to be split.
#' @param seps A vector of the separator character used in each column. If all
#' columns use the same character, you can enter that single character.
#' @param direction The desired form of the resulting \code{data.frame} or
#' \code{data.table}, either \code{'wide'} or \code{'long'}.  Defaults to
#' \code{'wide'}.
#' @param \dots Other arguments to \code{\link{cSplit}}.
#' @return A \code{data.table}.
#' @author Ananda Mahto
#' @seealso \code{\link{cSplit}}, for which this is simply a wrapper, and
#' \code{\link{concat.split}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.multiple}},
#' \code{\link{Reshape}}
#' @examples
#' 
#' temp <- head(concat.test)
#' concat.split.multiple(temp, split.cols = c("Likes", "Hates", "Siblings"),
#'                       seps = c(",", ";", ","))
#' concat.split.multiple(temp, split.cols = c("Likes", "Siblings"),
#'                       seps = ",", direction = "long")
#' \dontshow{rm(temp)}
#' 
#' @export concat.split.multiple
concat.split.multiple <- function(data, split.cols, seps = ",", 
                                  direction = "wide", ...) {
  message("This function is deprecated. Use `cSplit` instead.")
  cSplit(indt = data, splitCols = split.cols, 
         sep = seps, direction = direction, ...)
}
NULL
