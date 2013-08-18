#' Split concatenated cells in a \code{data.frame} into a condensed format
#' 
#' The default splitting method for \code{\link{concat.split}}. Uses
#' \code{\link{read.concat}} to do most of the processing.
#' 
#' 
#' @param data The source \code{data.frame}
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed An unused dummy argument to make the function compatible with
#' \code{\link{concat.split.expanded}}.
#' @return A \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{\link{read.concat}}, \code{\link{concat.split}},
#' \code{\link{concat.split.list}}, \code{\link{concat.split.expanded}},
#' \code{\link{concat.split.multiple}}
#' @examples
#' 
#' temp <- head(concat.test)
#' concat.split.compact(temp, "Likes")
#' concat.split.compact(temp, 4, ";")
#' concat.split.compact(temp, "Siblings", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @export concat.split.compact
concat.split.compact <- function(data, split.col, sep = ",", 
                                 drop = FALSE, fixed = NULL) {
  if (!is.character(data[split.col])) a <- as.character(data[[split.col]])
  else a <- data[[split.col]]
  
  t1 <- read.concat(a, names(data[split.col]), sep)
  if (isTRUE(drop)) cbind(data[othernames(data, split.col)], t1)
  else cbind(data, t1)
}
NULL











#' Split numeric concatenated values into their corresponding column position
#' 
#' "Expand" concatenated numeric values to their relevant position in a
#' \code{data.frame}.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param mode Can be either \code{"binary"} (where presence of a number in a
#' given column is converted to "1") or \code{"value"} (where the value is
#' retained and not recoded to "1").
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{strsplit} for allowing regular expressions to be
#' used.
#' @param fill Desired "fill" value. Defaults to \code{NA}.
#' @return A \code{data.frame}
#' @note When \code{mode = "binary"} is selected, the function calls
#' \code{\link{binaryMat}} to expand the values. When \code{mode = "value"} is
#' selected, the function calls \code{\link{valueMat}}.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.list}},
#' \code{\link{concat.split.compact}}, \code{\link{concat.split.multiple}},
#' \code{\link{binaryMat}}, \code{\link{valueMat}}
#' @examples
#' 
#' temp <- head(concat.test)
#' concat.split.expanded(temp, "Likes")
#' concat.split.expanded(temp, 4, ";")
#' concat.split.expanded(temp, 4, ";", mode = "value", drop = TRUE)
#' 
#' ## Note the warning
#' concat.split.expanded(temp, "Siblings", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @export concat.split.expanded
concat.split.expanded <- function(data, split.col, sep = ",", mode = NULL, 
                                  drop = FALSE, fixed = FALSE, fill = NA) {
  if (!is.character(data[split.col])) a <- as.character(data[[split.col]])
  else a <- data[[split.col]]
  
  b <- strsplit(a, sep, fixed = fixed)
  
  if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
    temp1 <- concat.split.compact(data, split.col =  split.col, sep = sep, 
                                  drop = drop, fixed = fixed)
    m <- paste("Your concatenated variable is a string.", 
               "We have used 'concat.split.compact' instead.", sep = "\n")
    message(m)
    Return <- "A"
  } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
    if (is.null(mode)) mode = "binary"
    nchars <- max(nchar(unlist(b)))
    temp1 <- switch(
      mode,
      binary = {
        temp <- binaryMat(b, fill = fill)
        colnames(temp) <- 
          sprintf(paste0(names(data[split.col]), "_%0", nchars, "d"), 1:ncol(temp))
        temp
      },
      value = {
        temp <- valueMat(b, fill = fill)
        colnames(temp) <- 
          sprintf(paste0(names(data[split.col]), "_%0", nchars, "d"), 1:ncol(temp))
        temp
      },
      stop("'mode' must be 'binary' or 'value'"))
    expandedNames <- colnames(temp1)
    temp1 <- cbind(data, temp1)
    Return <- "B"
  }
  
  if (isTRUE(drop) & Return == "B") temp1[c(othernames(data, split.col), 
                                            expandedNames)]
  else temp1
}
NULL











#' Split concatenated cells in a \code{data.frame} into a \code{list} format
#' 
#' Takes a column in a \code{data.frame} with multiple values, splits the
#' values into a \code{list}, and returns a new \code{data.frame}.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{\link{strsplit}} for allowing regular
#' expressions to be used.
#' @return A \code{data.frame} with the concatenated column split and added as
#' a \code{list}.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.multiple}}
#' @examples
#' 
#' temp <- head(concat.test)
#' str(concat.split.list(temp, "Likes"))
#' concat.split.list(temp, 4, ";")
#' concat.split.list(temp, 4, ";", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
#' @export concat.split.list
concat.split.list <- function(data, split.col, sep = ",", 
                              drop = FALSE, fixed = FALSE) {
  if (!is.character(data[split.col])) a <- as.character(data[[split.col]])
  else a <- data[[split.col]]
  
  varname <- paste(names(data[split.col]), "list", sep="_")
  b <- strsplit(a, sep, fixed = fixed)
  
  if (suppressWarnings(is.na(try(max(as.numeric(unlist(b))))))) {
    data[varname] <- list(
      lapply(lapply(b, as.character),
             function(x) gsub("^\\s+|\\s+$", "", x)))
  } else if (!is.na(try(max(as.numeric(unlist(b)))))) {
    data[varname] <- list(lapply(b, as.numeric))
  }
  if (isTRUE(drop)) data[othernames(data, split.col)]
  else data
}
NULL











#' Split concatenated cells in a \code{data.frame}
#' 
#' The \code{concat.split} function takes a column with multiple values, splits
#' the values into a \code{list} or into separate columns, and returns a new
#' \code{data.frame}.
#' 
#' \emph{structure} \itemize{ \item \code{"compact"} creates as many columns as
#' the maximum length of the resulting split. This is the most useful
#' general-case application of this function. \item When the input is numeric,
#' \code{"expanded"} creates as many columns as the maximum value of the input
#' data. This is most useful when converting to \code{mode = "binary"}. \item
#' \code{"list"} creates a single new column that is structurally a
#' \code{\link{list}} within a \code{\link{data.frame}}. } \emph{fixed}
#' \itemize{ \item When \code{structure = "expanded"} or \code{structure =
#' "list"}, it is possible to supply a a regular expression containing the
#' characters to split on.  For example, to split on \code{","}, \code{";"}, or
#' \code{"|"}, you can set \code{sep = ",|;|\|"} or \code{sep = "[,;|]"}, and
#' \code{fixed = FALSE} to split on any of those characters.}
#' 
#' @param data The source \code{data.frame}.
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
#' @param drop Logical (whether to remove the original variable from the output
#' or not). Defaults to \code{FALSE}.
#' @param fixed Is the input for the \code{sep} value \emph{fixed}, or a
#' \emph{regular expression}? See Details.
#' @param fill The "fill" value for missing values when \code{structure =
#' "expanded"}. Defaults to \code{NA}.
#' @note This is more of a "legacy" or "convenience" wrapper function
#' encompassing the features available in the separated functions of
#' \code{\link{concat.split.compact}}, \code{\link{concat.split.list}}, and
#' \code{\link{concat.split.expanded}}.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.list}},
#' \code{\link{concat.split.multiple}}
#' @references \itemize{ \item See
#' \url{http://stackoverflow.com/q/10100887/1270695} for some history of this
#' function, even though the solution is not used at all here. \item The
#' \code{"condensed"} setting was inspired by an answer from David Winsemius to
#' a question at Stack Overflow.  See:
#' \url{http://stackoverflow.com/a/13924245/1270695} }
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
#' mode = "value", drop = TRUE)
#' 
#' # Let's try splitting some strings... Same syntax
#' concat.split(temp, 3, drop = TRUE)
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
                         mode = NULL, drop = FALSE, fixed = FALSE, fill = NA) {
  
  Message <- paste(c("", "'mode' supplied but ignored.", 
                     "'mode' setting only applicable",
                     "when structure = 'expanded'"), collapse = "\n")
  temp <- switch(
    structure, 
    compact = {
      if (!is.null(mode)) warning(Message)
      concat.split.compact(data = data, split.col = split.col, 
                           sep = sep, drop = drop, fixed = fixed)
    },
    list = {
      if (!is.null(mode)) warning(Message)
      concat.split.list(data = data, split.col = split.col, 
                        sep = sep, drop = drop, fixed = fixed)
    },
    expanded = {
      concat.split.expanded(data = data, split.col = split.col, 
                            sep = sep, mode = mode, drop = drop, 
                            fixed = fixed, fill = fill)
    },
    stop("'structure' must be either 'compact', 'expanded', or 'list'"))
  temp
}
NULL











#' Split concatenated cells in a \code{data.frame} and optionally reshape the
#' output
#' 
#' This is an extended version of the \code{\link{concat.split.compact}}
#' function that allows the user to split multiple columns at once and
#' optionally use the \code{\link{Reshape}} function to convert the
#' \code{data.frame} into a "long" format.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param split.cols A vector of columns that need to be split.
#' @param seps A vector of the separator character used in each column. If all
#' columns use the same character, you can enter that single character.
#' @param direction The desired form of the resulting \code{data.frame}, either
#' \code{'wide'} or \code{'long'}.  Defaults to \code{'wide'}.
#' @return A \code{data.frame}. If \code{direction = "long"}, a
#' \code{data.frame} with additional attributes created by the
#' \code{\link{reshape}} function in base R.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.compact}},
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
                                  direction = "wide") {
  split.cols <- Names(data, split.cols)
  IDs <- othernames(data, split.cols)
  if (length(seps) == 1) seps <- rep(seps, length(split.cols))
  if (length(seps) != length(split.cols)) {
    stop("Verify you have entered the correct number of seps")
  }
  temp <- lapply(seq_along(split.cols), function(x) {
    concat.split(data[split.cols[x]], split.cols[x], seps[x], drop = TRUE)
  })
  
  out <- switch(
    direction, 
    wide = {
      cbind(data[IDs], do.call(cbind, temp))
    },
    long = {
      Reshape(cbind(data[IDs], do.call(cbind, temp)), id.vars = IDs, 
              var.stubs = split.cols, sep = "_")
    },
    stop("'direction' must be either 'wide' or 'long'.")
  )
  out[out == ""] <- NA
  out
}
NULL
