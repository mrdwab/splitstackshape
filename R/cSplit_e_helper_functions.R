#' Create a Numeric Matrix from a List of Values
#' 
#' Create a numeric matrix from a list of values.
#' 
#' Takes an input \code{list} and converts it to a binary, value, or count 
#' matrix.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param mode Either \code{"binary"}, \code{"value"}, or \code{"count"}. 
#' Defaults to \code{"binary"}.
#' @param fill The initializing fill value for the empty matrix. Defaults to
#' \code{NA} if \code{mode = "value"} and \code{0} for other modes.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{char_mat}}
#' @examples
#' 
#' invec <- c("1,2,4,5,6", "1,2,4,5,6", NA,
#'            "1,2,4,5,6,6,6", "-1,1,2,5,6", "1,2,5,6")
#' A <- strsplit(invec, ",")
#' num_mat(A)
#' num_mat(A, fill = 0)
#' num_mat(A, mode = "value")
#' num_mat(A, mode = "count")
#' 
#' @export
num_mat <- function (listOfValues, mode = "binary", fill = NULL) {
  len <- length(listOfValues)
  vec <- as.integer(unlist(listOfValues, use.names = FALSE))
  slvl <- seq.int(min(vec, na.rm = TRUE), max(vec, na.rm = TRUE))
  i.idx <- rep.int(seq_len(len), lengths(listOfValues))
  
  if (mode %in% c("binary", "value")) {
    if (is.null(fill)) {
      fill <- if (mode == "binary") 0L else NA_integer_
    }
    out <- matrix(as.integer(fill), nrow = len, ncol = length(slvl), 
                  dimnames = list(NULL, slvl))
    j.idx <- match(vec, slvl)
    out[na.omit(cbind(i.idx, j.idx))] <- switch(
      mode, binary = 1L, value = na.omit(vec), 
      stop("'mode' must be 'binary' or 'value'"))
  } else if (mode == "count") {
    out <- unclass(table(i.idx, factor(vec, slvl)))
    if (!is.null(fill)) {
      out <- replace(out, out == 0L, fill)
    }
  }
  setattr(out, "dimnames", unname(attr(out, "dimnames")))[]
}
NULL

#' Create a Binary Matrix from a List of Values
#' 
#' Create a binary matrix from a list of character values.
#' 
#' Takes an input \code{list} and converts it to a binary, value, or count 
#' matrix.
#' 
#' @param listOfValues A \code{list} of input values to be inserted in a
#' matrix.
#' @param mode Either \code{"binary"}, \code{"value"}, or \code{"count"}. 
#' Defaults to \code{"binary"}.
#' @param fill The initializing fill value for the empty matrix. Defaults to
#' \code{NA} if \code{mode = "value"} and \code{0} for other modes.
#' @return A \code{matrix}.
#' @author Ananda Mahto
#' @seealso \code{strsplit}, \code{\link{num_mat}}
#' @examples
#' 
#' invec <- c("rock,electro", "electro", "rock,jazz", NA, "jazz,jazz,jazz")
#' A <- strsplit(invec, ",")
#' char_mat(A)
#' char_mat(A, fill = 99)
#' char_mat(A, mode = "value")
#' char_mat(A, mode = "count")
#' 
#' @export
char_mat <- function (listOfValues, mode = "binary", fill = NULL) {
  len <- length(listOfValues)
  vec <- trim_vec(unlist(listOfValues, use.names = FALSE), attr = FALSE)
  lvl <- sort(unique(vec))
  i.idx <- rep.int(seq_along(listOfValues), lengths(listOfValues))
  
  if (mode %in% c("binary", "value")) {
    fill <- if (is.null(fill)) {
      if (mode == "binary") 0L else NA_integer_
    } else {
      fill
    }
    out <- matrix(as.integer(fill), nrow = len, ncol = length(lvl), 
                  dimnames = list(NULL, lvl))
    j.idx <- match(vec, lvl)
    out[na.omit(cbind(i.idx, j.idx))] <- switch(
      mode, binary = 1L, value = na.omit(vec), 
      stop("'mode' must be 'binary' or 'value'"))
  } else if (mode == "count") {
    out <- unclass(table(i.idx, factor(vec, lvl)))
    if (!is.null(fill)) {
      out <- replace(out, out == 0L, fill)
    }
  }
  setattr(out, "dimnames", unname(attr(out, "dimnames")))[]
}
NULL

#' Trim Whitespace from a List of Character Vectors
#' 
#' Trims whitespace from a list of character vectors.
#' 
#' Takes an input \code{list} of character vectors trims any leading or
#' trailing whitespace, and optionally relists the output.
#' 
#' @param x A \code{list} of character vectors.
#' @param relist Logical. Should the output also be a \code{list}? Defaults to
#' \code{TRUE}.
#' @param convert Logical. Should \code{type.convert} be used on the output?
#' Defaults to \code{FALSE}. 
#' @return A \code{list} or the unlisted values.
#' @note Empty list elements or empty characters (eg: \code{""}) are converted
#' to \code{NA}.
#' @author Ananda Mahto
#' @seealso \code{trimws}
#' @examples
#' 
#' L1 <- list(c(" A", "B   ", NA), NA, c("   A", NA, ""), "", "B   ")
#' trim_list(L1)
#' trim_list(L1, FALSE)
#' 
#' L2 <- list(c(" 1", "2   ", NA), NA, c("   1", NA, ""), "", "3   ")
#' trim_list(L2)
#' trim_list(L2, convert = TRUE)
#' 
#' @export
trim_list <- function(x, relist = TRUE, convert = FALSE) {
  x <- replace(x, lengths(x) == 0, NA_character_)
  y <- unlist(x, use.names = FALSE)
  y[!nzchar(y)] <- NA_character_
  out <- trim_vec(y, TRUE)
  if ((attr(out, "test") == "clean") & (!convert)) {
    x
  } else {
    setattr(out, "test", NULL)
    if (convert) out <- type.convert(out, as.is = TRUE)
    if (relist) {
      out <- data.table(out, rep.int(seq.int(length(x)), 
                                     lengths(x)))[, list(list(out)), V2]$V1
      if (is.null(names(x))) out else `names<-`(out, names(x))
    }
    out
  }
}
NULL

#' Trim Whitespace from a Character Vector
#' 
#' Trims whitespace from a character vector.
#' 
#' Takes an input character vector and trims any leading or trailing whitespace.
#' 
#' @param vec A character vector.
#' @param attr Logical. Should an attribute be set indicating that the the 
#' output was "clean" or has been "cleaned"? Defaults to \code{FALSE}. 
#' @return A vector of the trimmed values.
#' @note The \code{attr} argument is used to increase the efficiency of
#' \code{\link{trim_list}}.
#' @author Ananda Mahto
#' @seealso \code{trimws}, \code{\link{trim_list}}
#' @examples
#' 
#' V1 <- letters
#' trim_vec(V1)
#' trim_vec(V1, TRUE)
#' 
#' V2 <- c(" 1", "2   ", NA, "")
#' trim_vec(V2)
#' trim_vec(V2, TRUE)
#' 
#' @export
trim_vec <- function(vec, attr = FALSE) {
  if (!is.atomic(vec)) stop("This function is for character vectors only")
  if (any(endsWith(vec, " ") | startsWith(vec, " "), na.rm = TRUE)) {
    vec <- stringi::stri_trim_both(vec, "\\P{Zs}")
    vec <- if (attr) setattr(copy(vec), "test", "cleaned")[] else vec
  } else {
    vec <- if (attr) setattr(copy(vec), "test", "clean")[] else vec
  }
  vec
}
NULL
