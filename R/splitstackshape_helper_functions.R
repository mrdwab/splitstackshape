#' Extract All Names From a Dataset Other Than the Ones Listed
#' 
#' A convenience function for \code{setdiff(names(data),
#' -some_vector_of_names-)}.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param toremove The \code{names} you want to exclude.
#' @return A character vector of the remaining names.
#' @author Ananda Mahto
#' @seealso \code{\link{setdiff}}
#' @examples
#' 
#' mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' splitstackshape:::othernames(mydf, "a")
#' 
#' \dontshow{rm(mydf)}
#' 
othernames <- function(data, toremove) {
  setdiff(names(data), Names(data, toremove))
}
NULL



#' Dataset Names as a Character Vector, Always
#' 
#' A convenience function using either character vectors or numeric vectors to
#' specify a subset of \code{names} of a \code{data.frame}.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param invec The \code{names} you want.
#' @return A character vector of the desired names.
#' @author Ananda Mahto
#' @examples
#' 
#' mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' splitstackshape:::Names(mydf, c("a", "c"))
#' splitstackshape:::Names(mydf, c(1, 3))
#' 
#' \dontshow{rm(mydf)}
#' 
Names <- function(data, invec) {
  if (!is.numeric(invec)) invec <- match(invec, names(data))
  names(data)[invec]
}
NULL



#' Read Concatenated Character Vectors Into a data.frame
#' 
#' Originally a helper function for the \code{\link{concat.split.compact}} function. 
#' This function has now been effectively replaced by \code{\link{cSplit}}.
#' 
#' 
#' @param data The input data.
#' @param col.prefix The desired column prefix for the output
#' \code{data.frame}.
#' @param sep The character that acts as a delimiter.
#' @param \dots Other arguments to pass to \code{read.table}.
#' @return A \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{read.table}
#' @examples
#' 
#' vec <- c("a,b", "c,d,e", "f, g", "h, i, j,k")
#' splitstackshape:::read.concat(vec, "var", ",")
#' 
#' ## More than 5 lines the same
#' ## `read.table` would fail with this
#' vec <- c("12,51,34,17", "84,28,17,10", "11,43,28,15",
#' "80,26,17,91", "10,41,25,13", "97,35,23,12,13")
#' splitstackshape:::read.concat(vec, "var", ",")
#' 
#' \dontshow{rm(vec)}
#' 
read.concat <- function(data, col.prefix, sep, ...) {
  if (!is.character(data)) data <- as.character(data)
  zz <- textConnection(data)
  x <- count.fields(zz, sep = sep)
  close(zz)
  t1 <- read.table(text = data, sep = sep, fill = TRUE,
                   row.names = NULL, header = FALSE,
                   strip.white = TRUE,
                   col.names = paste("v", sequence(max(x))),
                   ...)
  names(t1) <- paste(col.prefix, seq(ncol(t1)), sep = "_")
  t1
}
NULL



#' Create a Numeric Matrix from a List of Values
#' 
#' Create a numeric matrix from a list of values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @param mode Either \code{"binary"} or \code{"value"}. Defaults to
#' \code{"binary"}.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{charMat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
#'            "1,2,4,5,6", "-1,1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' splitstackshape:::numMat(A)
#' splitstackshape:::numMat(A, fill = 0)
#' splitstackshape:::numMat(A, mode = "value")
#' 
#' \dontshow{rm(invec, A)}
#' 
numMat <- function(listOfValues, fill = NA, mode = "binary") {
  listOfValues <- lapply(listOfValues, as.integer)
  len  <- length(listOfValues)
  vec  <- unlist(listOfValues, use.names = FALSE)
  slvl <- seq(min(vec), max(vec))
  out  <- matrix(fill, nrow = len, ncol = length(slvl), dimnames = list(NULL, slvl))
  i.idx <- rep(seq_len(len), vapply(listOfValues, length, integer(1L)))
  j.idx <- match(vec, slvl)
  out[cbind(i.idx, j.idx)] <- switch(mode, binary = 1L, value = vec, 
                                     stop("'mode' must be 'binary' or 'value'"))
  out
}
NULL



#' Create a Binary Matrix from a List of Character Values
#' 
#' Create a binary matrix from a list of character values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @param mode Either \code{"binary"} or \code{"value"}. Defaults to
#' \code{"binary"}.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{numMat}}
#' @examples
#' 
#' invec <- c("rock,electro","electro","rock,jazz")
#' A <- strsplit(invec, ",")
#' splitstackshape:::charMat(A)
#' splitstackshape:::charMat(A, 0)
#' splitstackshape:::charMat(A, mode = "value")
#' 
#' \dontshow{rm(invec, A)}
#' 
charMat <- function(listOfValues, fill = NA, mode = "binary") {
  len   <- length(listOfValues)
  vec   <- unlist(listOfValues, use.names = FALSE)
  lvl   <- sort(unique(vec))
  out   <- matrix(fill, nrow = len, ncol = length(lvl), 
                  dimnames = list(NULL, lvl))
  i.idx <- rep(seq.int(len), vapply(listOfValues, length, integer(1L)))
  j.idx <- match(vec, lvl)
  out[cbind(i.idx, j.idx)] <- switch(mode, binary = 1L, value = vec, 
                                     stop("'mode' must be 'binary' or 'value'"))
  out
}
NULL



#' Split Basic Alphanumeric Strings Which Have No Separators
#' 
#' Used to split strings like "Abc8" into "Abc" and "8".
#' 
#' 
#' @param data The vector of strings to be split.
#' @param charfirst Is the string constructed with characters at the start or
#' numbers? Defaults to \code{TRUE}.
#' @return A \code{data.frame} with two columns, \code{.var} and
#' \code{.time_1}.
#' @note This is a helper function for the \code{\link{Stacked}} and
#' \code{\link{Reshape}} functions.
#' @author Ananda Mahto
#' @seealso \code{\link{strsplit}}
#' @examples
#' 
#' x <- paste0("Var", LETTERS[1:3], 1:3)
#' splitstackshape:::NoSep(x)
#' 
#' y <- paste0(1:3, "Var", LETTERS[1:3])
#' splitstackshape:::NoSep(y, charfirst = FALSE)
#' 
#' \dontshow{rm(x, y)}
#' 
NoSep <- function(data, charfirst = TRUE) {
  if (isTRUE(charfirst)) {
    Pattern <- "([[:alpha:]]+)([[:digit:]]+)"
    Names <- c(".var", ".time_1")
  } else {
    Pattern <- "([[:digit:]]+)([[:alpha:]]+)"
    Names <- c(".time_1", ".var")
  }
  setNames(data.frame(gsub(Pattern, "\\1", data), 
                      gsub(Pattern, "\\2", data)), Names)
}
NULL



#' Convert All Factor Columns to Character Columns
#' 
#' Sometimes, we forget to use the \code{stringsAsFactors} argument when using
#' \code{\link{read.table}} and related functions. By default, R converts
#' character columns to factors. Instead of re-reading the data, the
#' \code{\link{FacsToChars}} function will identify which columns are currently
#' factors, and convert them all to characters.
#' 
#' 
#' @param mydf The name of your \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{\link{read.table}}
#' @examples
#' 
#' ## Some example data
#' dat <- data.frame(title = c("title1", "title2", "title3"),
#'          author = c("author1", "author2", "author3"),
#'          customerID = c(1, 2, 1))
#' 
#' str(dat) # current structure
#' dat2 <- splitstackshape:::FacsToChars(dat)
#' str(dat2) # Your new object
#' str(dat)  # Original object is unaffected
#' 
#' \dontshow{rm(dat, dat2, dat_copy)}
#' 
FacsToChars <- function(mydf) {
  mydf[sapply(mydf, is.factor)] <- 
    lapply(mydf[sapply(mydf, is.factor)], as.character)
  mydf
}
NULL

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
NULL

.collapseMe <- function(invec, atStart = TRUE) {
  if (isTRUE(atStart)) paste(sprintf("^%s", invec), collapse = "|")
  else paste(sprintf("%s$", invec), collapse = "|")
}
NULL

.stripWhite <- function(invec, delim = ",") {
  gsub("^\\s+|\\s+$", "",
       gsub(sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                    delim, delim, delim), delim, invec))
}
NULL

vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
NULL

.noEmpty <- function(invec) {
  invec[invec != ""]
}
NULL

.pad <- function(invec) {
  nchars <- max(nchar(invec))
  sprintf(paste0("%0", nchars, "d"), invec)
}
NULL
