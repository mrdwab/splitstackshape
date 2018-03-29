#' Split Concatenated Values into Separate Values
#' 
#' The `cSplit` function is designed to quickly and conveniently split
#' concatenated data into separate values.
#' 
#' 
#' @param indt The input `data.frame` or `data.table`.
#' @param splitCols The column or columns that need to be split.
#' @param sep The values that serve as a delimiter *within* each column. This 
#' can be a single value if all columns have the same delimiter, or a vector of 
#' values *in the same order as the delimiters in each of the `splitCols`*.
#' @param direction The desired direction of the results, either `"wide"`
#' or `"long"`.
#' @param fixed Logical. Should the split character be treated as a fixed
#' pattern (`TRUE`) or a regular expression (`FALSE`)? Defaults to `TRUE`.
#' @param drop Logical. Should the original concatenated column be dropped?
#' Defaults to `TRUE`.
#' @param stripWhite Logical. If there is whitespace around the delimiter in
#' the concatenated columns, should it be stripped prior to splitting? Defaults
#' to `TRUE`.
#' @param makeEqual Logical. Should all groups be made to be the same length?
#' Defaults to `FALSE`.
#' @param type.convert Logical. Should [utils::type.convert()] be used to convert
#' the result of each column? This would add a little to the execution time.
#' @return A `data.table` with the values split into new columns or rows.
#' @note The `cSplit` function replaces most of the earlier `concat.split*` 
#' functions. The earlier functions remain for compatability purposes, but now 
#' they are essentially wrappers for the `cSplit` function.
#' 
#' @author Ananda Mahto
#' @seealso [concat.split()]
#' @examples
#' 
#' ## Sample data
#' temp <- head(concat.test)
#' 
#' ## Split the "Likes" column
#' cSplit(temp, "Likes")
#' 
#' ## Split the "Likes" and "Hates" columns --
#' ##   they have different delimiters...
#' cSplit(temp, c("Likes", "Hates"), c(",", ";"))
#' 
#' ## Split "Siblings" into a long form...
#' cSplit(temp, "Siblings", ",", direction = "long")
#' 
#' ## Split "Siblings" into a long form, not removing whitespace
#' cSplit(temp, "Siblings", ",", direction = "long", stripWhite = FALSE)
#' 
#' ## Split a vector
#' y <- c("a_b_c", "a_b", "c_a_b")
#' cSplit(data.frame(y), "y", "_")
#' 
#' @export cSplit
cSplit <- function(indt, splitCols, sep = ",", direction = "wide", 
                   fixed = TRUE, drop = TRUE, 
                   stripWhite = TRUE, makeEqual = NULL, 
                   type.convert = TRUE) {
  
  if (!is.data.table(indt)) indt <- as.data.table(indt)
  else indt <- copy(indt)

  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (any(!vapply(indt[, splitCols, with = FALSE],
                  is.character, logical(1L)))) {
    indt[, eval(splitCols) := lapply(.SD, as.character),
         .SDcols = splitCols]
  }
  
  if (length(sep) == 1) 
    sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) {
    stop("Verify you have entered the correct number of sep")
  }

  if (isTRUE(stripWhite)) {
    indt[, eval(splitCols) := mapply(function(x, y) 
      .stripWhite(x, y), 
      indt[, splitCols, with = FALSE], sep,
      SIMPLIFY = FALSE)]
  }  
  
  X <- lapply(seq_along(splitCols), function(x) {
    strsplit(indt[[splitCols[x]]], split = sep[x], fixed = fixed)
  })
  
  if (direction == "long") {
    if (is.null(makeEqual)) {
      IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
      makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                          FALSE, TRUE)
    }
  } else if (direction == "wide") {
    if (!is.null(makeEqual)) {
      if (!isTRUE(makeEqual)) {
        message("makeEqual specified as FALSE but set to TRUE")
        makeEqual <- TRUE
      }
      makeEqual <- TRUE
    } else {
      makeEqual <- TRUE
    }
  }
  if (isTRUE(makeEqual)) {
    SetUp <- lapply(seq_along(X), function(y) {
      A <- vapply(X[[y]], length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(X[[y]]))
    })    
    Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]), 
                       use.names = FALSE))
    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })
    if (direction == "wide") {
      X <- lapply(seq_along(X), function(x) {
        colnames(X[[x]]) <- paste(splitCols[x], 
                                  .pad(sequence(ncol(X[[x]]))), 
                                  sep = "_")
        X[[x]]
      })
      
      if (isTRUE(type.convert)) {
        X <- lapply(seq_along(X), function(x) {
          as.data.table(X[[x]])[, lapply(.SD, type.convert)]
        })
      }
      
      if (isTRUE(drop)) {
        cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
      } else {
        cbind(indt, do.call(cbind, X))
      }
      
    } else {
      indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
      X <- lapply(X, function(y) {
        if (isTRUE(type.convert)) type.convert(as.vector(t(y)))
        else as.vector(t(y))
      })
      indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
    }  
  } else {
    Rep <- vapply(X[[1]], length, integer(1L))
    indt <- indt[rep(sequence(nrow(indt)), Rep)]
    indt[, eval(splitCols) := lapply(X, function(x) {
      if (isTRUE(type.convert)) type.convert(unlist(x, use.names = FALSE))
      else unlist(x, use.names = FALSE)
    })][]
  }
}
NULL
