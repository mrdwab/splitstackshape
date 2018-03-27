#' @name unlist_cols
#' @rdname unlist_cols
#' @title Unlists or Flattens List Columns in a `data.table`
#' 
#' @description Unlists columns stored as `list`s into a long form or flattens
#' them into a wide form.
#' 
#' @param indt The input `data.table`.
#' @param listCols The columnst to unlist or flatten.
#' @param drop Logical. Should the original column be dropped? Defaults to `TRUE`.
#' @param makeEqual Logical. Should the lengths of values be made equal before 
#' converting to the long form?
#' @param direction Use `"long"` for unlisting behavior, and `"wide"` for 
#' flattening behavior.
#' @return A `data.table`.
#' @author Ananda Mahto
NULL



#' @rdname unlist_cols
#' 
#' @export 
#' @aliases listCol_l
listCol_l <- function(indt, listCols, drop = TRUE, makeEqual = FALSE) {
  ..cols <- ._ID1 <- ._ID2 <- patterns <- NULL
  if (is.numeric(listCols)) listCols <- names(indt)[listCols]
  temp <- listCol_w(indt, listCols, drop, makeEqual = TRUE)[, ._ID1 := .I]
  COLS <- if (drop) listCols else sprintf("%s_ul", listCols)
  out <- set(suppressWarnings(melt(
    temp, measure = patterns(sprintf("^%s_\\d+", listCols)),
    value.name = COLS)), j = "variable", value = NULL)
  out[, ._ID2 := seq.int(.N), ._ID1]
  setorderv(out, c("._ID1", "._ID2"), na.last = TRUE)
  out <- if (makeEqual) out else out[!(long_fixer(out, COLS) & ._ID2 != 1L)]
  set(out, j = c("._ID1", "._ID2"), value = NULL)[]
}
NULL

#' @rdname unlist_cols
#' 
#' @export 
#' @aliases listCol_w
listCol_w <- function(indt, listCols, drop = TRUE, makeEqual = FALSE) {
  indt <- setDT(copy(indt))
  if (is.numeric(listCols)) listCols <- names(indt)[listCols]
  
  for (i in seq_along(listCols)) {
    temp <- data.table::transpose(indt[[listCols[i]]])
    set(indt, j = sprintf("%s_%s", listCols[i], seq_along(temp)), value = temp)
    if (drop) set(indt, j = listCols[i], value = NULL)
  }
  
  if (makeEqual) make_equal(indt, listCols, verbose = FALSE)[] else indt[]
}
NULL

#' @rdname unlist_cols
#' 
#' @examples 
#' 
#' dat <- data.frame(A = 1:3, B = I(list(1:2, 1:3, 4)), C = I(list(1, 1:2, 1:4)))
#' listCol_l(dat, "B")
#' listCol_w(dat, "B")
#' 
#' # Demonstration of `makeEqual` behavior
#' unlist_cols(dat, c("B", "C"), direction = "wide", makeEqual = FALSE)
#' unlist_cols(dat, c("B", "C"), direction = "wide", makeEqual = TRUE)
#' unlist_cols(dat, c("B", "C"), direction = "long", makeEqual = FALSE)
#' unlist_cols(dat, c("B", "C"), direction = "long", makeEqual = TRUE)
#' 
#' @export 
#' @aliases unlist_cols
unlist_cols <- function(indt, listCols, direction = "wide", drop = TRUE, 
                        makeEqual = FALSE) {
  switch(direction,
         wide = listCol_w(indt, listCols, drop, makeEqual),
         long = listCol_l(indt, listCols, drop, makeEqual),
         stop("direction must be 'long' or 'wide'"))
}
NULL
