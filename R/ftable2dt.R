#' @name ftable2dt
#' @rdname ftable2dt
#' @title Convert an `ftable` or `array` Object to a `data.table`
#' 
#' @description While convenient methods exist for converting `table`s and other 
#' objects to `data.tables`s, such methods do not exist for converting an 
#' `ftable` to a `data.table`. An `ftable` is essentially a `matrix` with 
#' `attributes` for the rows and columns, which can be nested.
#' 
#' @param x The input `ftable` or `array`.
#' @param direction Should the reslut be "wide" (with multiple measurement.
#' columns) or "long" (with a single measurement column)? Defaults to `"wide"`.
#' @return A `data.table`
#' @author Ananda Mahto
#' @references <http://stackoverflow.com/a/11143126/1270695>
#' @note If the array has no `dimnames`, names would be added using the
#' [base::provideDimnames()] function. Defaults to [reshape2::melt()] if the 
#' input is a simple matrix and not a multidimensional array.
#' 
#' `ftable2dt` and `array2dt` are identical.
#' @examples
#' 
#' x <- ftable(Titanic, row.vars = 1:3)
#' x
#' ftable2dt(x)
#' ftable2dt(x, direction = "long")
#' 
#' dims <- c(2, 1, 2, 3, 2)
#' set.seed(1)
#' M <- `dim<-`(sample(100, prod(dims), TRUE), dims)
#' N <- O <- `dimnames<-`(M, lapply(dims, function(x) 
#'                                  c(letters, LETTERS)[seq_len(x)]))
#' names(attributes(O)$dimnames) <- c("first", "second", "third", 
#'                                    "fourth", "fifth")
#' 
#' array2dt(M)
#' array2dt(N)
#' array2dt(O)
#' array2dt(M, "long")
#' array2dt(N, "long")
#' array2dt(O, "long")
#' 
#' x <- array(1:3, c(2,4,4))
#' 
#' array2dt(x)
#' 
#' @export ftable2dt array2dt 
#' @aliases ftable2dt array2dt
ftable2dt <- array2dt <- function(x, direction = "wide") {
  if (!is.array(x)) stop("input must be an array")
  dims <- dim(x)
  if (length(dims) == 1L) {
    stop("nothing to do here....")
  } else if (length(dims) == 2L & (!any(class(x) %in% "ftable"))) {
    switch(direction, 
           wide = as.data.table(x),
           long = setDT(melt(x))[],
           stop("direction must be 'wide' or 'long'"))
  } else {
    FIX <- (!any(names(attributes(x)) %in% c("dimnames", "row.vars")))
    if (is.null(dimnames(x))) {
      x <- provideDimnames(x, base = list(
        as.character(seq_len(max(dims)))))
    }
    FT <- if (any(class(x) %in% "ftable")) x else ftable(x)
    temp <- ftablewide(FT, FIX = FIX)
    switch(direction,
           long = ftablelong(temp, FIX = FIX)[],
           wide = setorderv(temp[["Data"]], temp[["Names"]])[],
           stop("direction must be 'wide' or 'long'"))
  }
}
NULL

ftablewide <- function(FT, FIX = TRUE) {
  ft_attr <- attributes(FT)
  rows <- do.call(CJ, c(ft_attr$row.vars, sorted = FALSE))
  if (is.null(names(ft_attr$row.vars))) setnames(
    rows, paste0("V", seq_len(ncol(rows))))
  Nam <- names(rows)
  cols <- as.data.table(setattr(FT, "class", "matrix"))
  setnames(cols, do.call(paste, c(do.call(CJ, c(
    ft_attr$col.vars, sorted = FALSE)), sep = "_")))
  temp <- data.table(rows, cols)
  if (isTRUE(FIX)) temp[, (Nam) := lapply(.SD, as.integer), .SDcols = Nam]
  list(Attributes = ft_attr, Names = Nam, Data = temp)
}
NULL

ftablelong <- function(inlist, FIX = TRUE) {
  temp <- melt(inlist[["Data"]], id.vars = inlist[["Names"]], 
               variable.factor = FALSE)
  if (isTRUE(FIX)) set(temp, i = NULL, j = match("variable", names(temp)), 
                       value = as.integer(temp[["variable"]]))
  varName <- names(inlist[["Attributes"]]$col.vars)
  varName <- if (is.null(varName)) paste0("V", length(inlist[[2]])+1) else varName
  setnames(temp, "variable", varName)
}
NULL