#' @name t_split
#' @rdname t_split
#' @title Split Concatenated Vectors into Multi-column `data.table`s
#' 
#' @description Takes an input of a single vector and splits on a delimiter or
#' a regular expression to create multiple columns.
#' 
#' @param vec The input vector.
#' @param sep The character or regular expression on which to split each value.
#' @param fixed Logical. Is the `sep` value fixed or a regular expression?
#' Defaults to `TRUE`.
#' @param stripWhite Logical. Should leading and trailing whitespace be stripped
#' from the output? Defaults to `TRUE`.
#' @param type.convert Logical. Should the output be converted using the
#' [utils::type.convert()] function? Defaults to `TRUE`.
#' @param prefix Single character string indicating the desired column prefix
#' for the resulting column names. Defaults to `NULL` at which point names are
#' simply `V1` to `Vn`. If a prefix is supplied the resulting names are in the
#' form of `prefix_column_number`.
#' @return A `data.table`.
#' @note `t_split()` and `f_split()` may return different number of columns if a
#' `sep` value is found at the end of a string being split.
#' 
#' `t_split()` is automatically called with a `message` if:
#' 
#' * the result of `f_split()` is a single column.
#' * `fixed = FALSE`.
#' * the `sep` provided is more than 1 character long, or is equal to `"."` or `""`.
#' @author Ananda Mahto
NULL

#' @rdname t_split
#' @examples
#' 
#' vec <- c("b;c;d", "d;ef", "d;ef", "", NA, "x;yz")
#' t_split(vec, ";")
#' t_split(vec, ";", prefix = "new")
#' 
#' @export
#' @aliases t_split
t_split <- function(vec, sep, fixed = TRUE, stripWhite = TRUE, 
                    type.convert = TRUE, prefix = NULL) {
  temp <- tstrsplit(vec, sep, fixed)
  if (!stripWhite & type.convert) {
    message("type.convert requires stripWhite = TRUE. Setting type.convert = FALSE.")
  }
  if (stripWhite) temp <- trim_list(temp, convert = type.convert)
  if (is.null(prefix)) {
    setDT(temp)[]
  } else {
    setnames(setDT(temp), sprintf("%s_%d", prefix, seq_along(temp)))[]
  }
}
NULL

#' @rdname t_split
#' @examples 
#' 
#' vec <- c("rock,electro", "rock,jazz", NA, "jazz,jazz,jazz")
#' f_split(vec, ",")
#' f_split(vec, ",", prefix = "new")
#' 
#' # Expect messages on the following
#' vec <- c("rockSEPelectro", "rockSEPjazz", NA, "jazzSEPjazzSEPjazz")
#' f_split(vec, "SEP")
#' f_split(vec, "[A-Z]+", fixed = FALSE)
#' 
#' @export
#' @aliases f_split
f_split <- function(vec, sep, fixed = TRUE, stripWhite = TRUE, 
                    type.convert = TRUE, prefix = NULL) {
  if ((sep %in% c("", ".")) | (nchar(sep) > 1L) | (!fixed)) {
    message("Unsupported `sep`. Splitting with `t_split` instead.")
    temp <- t_split(vec, sep, fixed, stripWhite, type.convert)
  } else {
    
    temp <- if (packageVersion("data.table") < "1.10.5") {
      VEC <- as.character(vec)
      ana <- if (anyNA(VEC)) is.na(VEC) else NULL
      anb <- !nzchar(VEC)

      if (is.null(ana) & !any(anb)) {
        VEC <- if (requireNamespace("stringi", quietly = TRUE)) {
          stringi::stri_flatten(VEC, collapse = "\n")
        } else {
          # nocov start
          .strflat(VEC)
          # nocov end
        }
      } else {
        if (!is.null(ana)) VEC[which(ana)] <- sep
        if (any(anb)) VEC[which(anb)] <- sep
        VEC <- if (requireNamespace("stringi", quietly = TRUE)) {
          stringi::stri_flatten(VEC, collapse = "\n")
        } else {
          # nocov start
          .strflat(VEC)
          # nocov end
        }
      }
      fread(VEC, sep = sep, fill = TRUE, 
            blank.lines.skip = FALSE, header = FALSE,
            colClasses = if (!type.convert) "character" else NULL,
            encoding = "UTF-8",
            strip.white = stripWhite)[, lapply(
              .SD, function(x) replace(x, x ==  "", NA))]
    } else {
      vec <- as.character(vec)
      if (trim_vec(vec[length(vec)]) == "") {
        vec[length(vec)] <- NA
      } 
      fread(text = vec, sep = sep, fill = TRUE, 
            blank.lines.skip = FALSE, header = FALSE, nrows = length(vec),
            colClasses = if (!type.convert) "character" else NULL,
            encoding = "UTF-8",
            strip.white = stripWhite, logical01 = FALSE)[, lapply(
              .SD, function(x) replace(x, x ==  "", NA))]
    }
    
    if (length(temp) == 1L) {
      message("Expected more than 1 column. Trying with `t_split`.")
      temp <- t_split(vec, sep, fixed, stripWhite, type.convert, prefix)
    } 
  }
  
  if (is.null(prefix)) {
    temp[]
  } else {
    setnames(temp, sprintf("%s_%d", prefix, seq_along(temp)))[]
  }
}
NULL
