#' Split Columns Sequentially to Cartesian Product
#' 
#' @description Splits each requested column one at a time, resulting in the Cartesian
#' Product of the split data.
#' 
#' @param indt The input dataset.
#' @param splitCols The columns to be split.
#' @param sep The separator on which to split each column.
#' @param fixed Logical. Should the split character be treated as a fixed pattern 
#' (`TRUE`) or a regular expression (`FALSE`)? Defaults to `TRUE`.
#' @param stripWhite Logical. Should leading and whitespace be stripped from the
#' output? Defaults to `TRUE`.
#' @param type.convert Logical. Should [utils::type.convert()] be used to convert
#' the result of each column? This might add a little bit of time to the 
#' execution time.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [cSplit()], [cartesian_unlist()]
#' 
#' @examples
#' DC <- data.frame(AB = c("A", "B"), V1 = c("AB,BW", "x,y,z"), V2 = c("1,2,3", "4,5,6,7"))
#' cartesian_split(DC, c("V1", "V2"))
#' @export 
cartesian_split <- function(indt, splitCols, sep = ",", fixed = TRUE, 
                             stripWhite = TRUE, type.convert = TRUE) {
  if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
  if (length(sep) == 1L) sep <- rep(sep, length(splitCols))
  if (length(sep) != length(splitCols)) stop("Wrong number of sep supplied")
  
  for (i in seq_along(splitCols)) {
    indt <- suppressMessages(
      cSplit(indt, splitCols[i], sep[i], fixed, stripWhite, type.convert, direction = "long"))
  }
  
  indt[]
}
NULL

#' Unlist a List Sequentially to Cartesian Product
#' 
#' @description Unlists a list sequentially to resulting in the Cartesian Product 
#' of the list elements.
#' 
#' @param inlist The input list.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [cartesian_split()]
#' 
#' @examples
#' L2 <- list(X1 = list("A", c("A", "B"), "X", NULL),
#'            X2 = list(NULL, c(1, 2, 3), c(1, 2), c(1, 2, 3, 4)),
#'            X3 = list(c("a", "b"), "c", "d", c("e", "f", "g")))
#' cartesian_unlist(L2)
#' @export 
cartesian_unlist <- function(inlist) {
  LEN <- lengths(inlist)
  MLen <- max(LEN)
  outlist <- lapply(seq_along(inlist), function(x) {
    if (LEN[x]) length(inlist[[x]]) <- MLen
    trim_list(inlist[[x]], convert = TRUE)
  })
  if (!is.null(names(inlist))) outlist <- setNames(outlist, names(inlist))
  lst_id <- NULL
  DT <- as.data.table(outlist)[, lst_id := seq.int(MLen)]
  setcolorder(DT, move_me(names(DT), "lst_id first"))
  out <- Reduce(function(x, y) x[y, allow.cartesian = TRUE, on = "lst_id"],
                lapply(setdiff(names(DT), "lst_id"), function(COL)
                  DT[, list(unlist(get(COL))), by = "lst_id"]))
  setnames(out, names(DT))[]
}