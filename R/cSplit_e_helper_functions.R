#' @name num_mat
#' @rdname num_mat
#' @title Create Binary, Value, or Count Matrix from a List
#' 
#' @description Creates a binary, value, or count matrix from a list of values.
#' 
#' Takes an input `list` and converts it to a binary, value, or count matrix.
#' 
#' @param listOfValues A `list` of input values to be inserted in a matrix.
#' @param mode Either `"binary"`, `"value"`, or `"count"`. Defaults to `"binary"`.
#' @param fill The initializing fill value for the empty matrix. Defaults to `NA` 
#' if `mode = "value"` and `0` for other modes.
#' @return A `matrix`.
#' @author Ananda Mahto
NULL

#' @rdname num_mat
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
#' @aliases num_mat
num_mat <- function (listOfValues, mode = "binary", fill = NULL) {
  listOfValues <- trim_list(listOfValues)
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

#' @rdname num_mat
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
#' @aliases char_mat
char_mat <- function (listOfValues, mode = "binary", fill = NULL) {
  len <- length(listOfValues)
  listOfValues <- trim_list(listOfValues)
  vec <- unlist(listOfValues, use.names = FALSE)
  # vec <- trim_vec(unlist(listOfValues, use.names = FALSE), attr = FALSE)
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


#' @name trim_list
#' @rdname trim_list
#' @title Trims Whitespace from Lists of Character Vectors
#' 
#' @description Trims whitespace from character vectors and lists of character
#' vectors. `trim_vec` takes a character vector and trims any leading or trailing
#' whitespace. `trim_list` does the same for vectors in a `list`, optionally
#' retaining the `list` structure or flattening the `list` to a simple vector.
#' 
#' @param x A `list` of character vectors.
#' @param relist Logical. Should the output also be a `list`? Defaults to `TRUE`.
#' @param convert Logical. Should [utils::type.convert()] be used on the output?
#' Defaults to `FALSE`. 
#' @param vec A character vector.
#' @param attr Logical. Should an attribute be set indicating that the the 
#' output was `"clean"` or has been `"cleaned"`? Defaults to `FALSE`. 
#' @return A `list` or a vector of the trimmed values, possibly converted for
#' `trim_list()` or a vector of the trimmed values for `trim_vec()`.
#' @note Empty list elements or empty characters (eg: `""`) are converted to `NA`.
#' @note The `attr` argument in `trim_vec()` is used to increase the efficiency 
#' of `trim_list()` by returning the original object if no leading or trailing
#' whitespace was detected.
#' @seealso [base::trimws()]
#' @author Ananda Mahto
NULL

#' @rdname trim_list
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
#' @aliases trim_list
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
      V2 <- NULL
      out <- data.table(out, rep.int(seq.int(length(x)), 
                                     lengths(x)))[, list(list(out)), V2]$V1
      if (is.null(names(x))) out else `names<-`(out, names(x))
    }
    out
  }
}
NULL


#' @rdname trim_list
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
#' @aliases trim_vec
trim_vec <- function(vec, attr = FALSE) {
  if (!is.atomic(vec)) stop("This function is for character vectors only")
  if (any(endsWith(vec, " ") | startsWith(vec, " "), na.rm = TRUE)) {
    vec <- if (requireNamespace("stringi", quietly = TRUE)) {
      stringi::stri_trim_both(vec, "\\P{Zs}")
    } else {
      .tws(vec)
    }
    vec <- if (attr) setattr(copy(vec), "test", "cleaned")[] else vec
  } else {
    vec <- if (attr) setattr(copy(vec), "test", "clean")[] else vec
  }
  vec
}
NULL
