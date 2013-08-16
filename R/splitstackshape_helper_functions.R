#' Extract all names from a \code{data.frame} other than the one listed
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
  setdiff(names(data), names(data[toremove]))
}
NULL





#' Read concatenated character vectors into a \code{data.frame}
#' 
#' A helper function for the \code{\link{concat.split.compact}} function.
#' 
#' 
#' @param data The input data.
#' @param col.prefix The desired column prefix for the output
#' \code{data.frame}.
#' @param sep The character that acts as a delimiter.
#' @return A \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{read.table}
#' @examples
#' 
#' vec <- c("a,b", "c,d,e", "f, g", "h, i, j,k")
#' splitstackshape:::read.concat(vec, "var", ",")
#' 
#' \dontshow{rm(vec)}
#' 
read.concat <- function(data, col.prefix, sep) {
  if (!is.character(data)) data <- as.character(data)
  x <- count.fields(textConnection(data), sep = sep)
  t1 <- read.table(text = data, sep = sep, fill = TRUE,
                   row.names = NULL, header = FALSE,
                   blank.lines.skip = FALSE, strip.white = TRUE,
                   col.names = paste("v", sequence(max(x))))
  names(t1) <- paste(col.prefix, seq(ncol(t1)), sep = "_")
  t1
}
NULL





#' Create a binary matrix from a list of values
#' 
#' Create a binary matrix from a list of values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{valueMat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
#'            "1,2,4,5,6", "1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' splitstackshape:::binaryMat(A)
#' splitstackshape:::binaryMat(A, "ZZZ")
#' 
#' \dontshow{rm(invec, A)}
#' 
binaryMat <- function(listOfValues, fill = NA) {
  listOfValues <- lapply(listOfValues, as.numeric)
  ncol <- max(unlist(listOfValues))
  nrow <- length(listOfValues)
  m <- matrix(fill, nrow = nrow, ncol = ncol)
  for (i in 1:nrow) {
    m[i, listOfValues[[i]]] <- 1
  }
  m
}
NULL





#' Create a binary matrix from a list of values
#' 
#' Create a binary matrix from a list of values
#' 
#' This is primarily a helper function for the \code{\link{concat.split}}
#' function when creating the "expanded" structure. The input is anticipated to
#' be a \code{list} of values obtained using \code{\link{strsplit}}.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param fill The initializing fill value for the empty matrix.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{binaryMat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
#'            "1,2,4,5,6", "1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' splitstackshape:::valueMat(A)
#' splitstackshape:::valueMat(A, "ZZZ")
#' 
#' \dontshow{rm(invec, A)}
#' 
valueMat <- function(listOfValues, fill = NA) {
  listOfValues <- lapply(listOfValues, as.numeric)
  ncol <- max(unlist(listOfValues))
  nrow <- length(listOfValues)
  m <- matrix(fill, nrow = nrow, ncol = ncol)
  for (i in 1:nrow) {
    m[i, listOfValues[[i]]] <- listOfValues[[i]]
  }
  m
}
NULL





#' Split basic alphanumeric strings which have no separators
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



#' Convert all \code{factor} columns to \code{character} columns in a
#' \code{data.frame}
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
