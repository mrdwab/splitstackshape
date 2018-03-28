#' Reshape Unbalanced Wide Data Into a Long Form
#' 
#' @description Takes unbalanced wide data (where measurements may not all have
#' the same number of time or observation variables) and converts it into a long
#' format. 
#' @param indt The input `data.table`.
#' @param stubs The prefixes or suffixes of the varying groups. This can be a
#' `name`d character vector. See "Details".
#' @param ids The columns to be used as "ID" variables. Defaults to `NULL` at
#' which point, all names which are not identified as part of the varying
#' columns are used.
#' @param value.name An alternative name to be used for the resulting 
#' consolidated columns. See "Details".
#' @param variable.name An alternative name to be used instead of "variable" to
#' indicate the measurement or observation period
#' @param end_stub Logical. Is the stub at the start of the variable name (eg: 
#' "var.1") or at the end of the variable name ("1.var")? Defaults to `FALSE`
#' indicating that a naming pattern such as "var.1" is expected.
#' @param keep.all Logical. Should all columns be kept, or only those specified
#' in the combination of column names from "stubs" and "ids"? Defaults to `TRUE`.
#' @return A `data.table`.
#' @details 
#' 
#' The names of the resulting columns can be provided in a few different ways.
#' 
#' * If nothing is specified, the provided "stubs" are used as the new column 
#' names.
#' * If the "stubs" value is a named vector, the names of each vector element
#' is used for the new column names.
#' * If "variable.name" is specified, the names provided there will be used for
#' the new column names.
#' * If "stubs" is a named vector *and* "variable.name" has been specified,
#' names are taken from the names in the "stubs" vector and those from 
#' "variable.name" are ignored.
#' 
#' @author Ananda Mahto
#' @examples
#' 
#' DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"),
#'                  varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
#'                  varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
#'                  varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
#' reshape_long(DF, c("varA", "varB", "varC"))
#' reshape_long(DF, c(colA = "varA", colB = "varB", colC = "varC"))
#'  
#' @export
#' @aliases reshape_long
reshape_long <- function(indt, stubs, ids = NULL, value.name = NULL, 
                         variable.name = NULL, end_stub = FALSE, 
                         keep.all = TRUE) {

  indt <- setDT(copy(indt))
  check <- all_names(names(indt), stubs, end_stub, ids, keep_all = keep.all)
  
  if (!is.null(ids)) check[["id_names"]] <- ids
  
  if (length(check[["miss"]]) > 0L) {
    nat <- vapply(unname(check[["stubs"]]), function(x) {
      typeof(indt[[grep(x, names(indt))[[1]]]][1])
    }, character(1L))
    
    for (i in seq_along(nat)) {
      COLS <- grep(names(nat[i]), check[["miss"]], value = TRUE)
      if (length(COLS) > 0L) indt[, (COLS) := NA_type(nat[i])]
    } 
  } 
  
  setcolorder(indt, check[["full_names"]])
  
  valn <- if (is.null(names(stubs))) {
    if (is.null(value.name)) stubs else value.name
  } else {
    names(stubs)
  }
  
  patterns <- NULL
  
  varn <- if (is.null(variable.name)) "variable" else variable.name
  out <- melt(indt, measure = patterns(stubs), 
              id.vars = check[["id_names"]],
              value.name = valn,
              variable.name = varn)
  setattr(out[[varn]], "levels", check[["levs"]])
  out
}
NULL


#' Reshapes Wide Data According to Variable Stub Values
#' 
#' A wrapper around the [reshape_long()] function. Will be removed from future
#' versions of "splitstackshape".
#' 
#' 
#' @param indt The input `data.table`.
#' @param id.vars The columns to be used as "ID" variables. Defaults to `NULL`, 
#' at which point, all names which are not identified as variable groups are 
#' used as the identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep Legacy argument. No longer implemented.
#' @param keep.all Logical. Should all the variables in the source data be kept 
#' (`keep.all = TRUE`) or only those which comprise the `id.vars` and split data
#' from the `var.stubs` (`keep.all = FALSE`).
#' @param end_stub Logical. Is the stub at the start of the variable name (eg: 
#' "var.1") or at the end of the variable name ("1.var")? Defaults to `FALSE`
#' indicating that a naming pattern such as "var.1" is expected.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @seealso [reshape_long()]
#' @examples
#' 
#' DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"),
#'                  varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
#'                  varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
#'                  varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
#' merged.stack(DF, var.stubs = c("varA", "varB", "varC"))
#' 
#' @export merged.stack
merged.stack <- function(indt, id.vars = NULL, var.stubs, keep.all = TRUE, 
                         end_stub = FALSE, sep = NULL) {
  message("This function is deprecated. Use `reshape_long()` instead.")
  if (!missing(sep)) .NotYetUsed("sep", error = FALSE)
  reshape_long(indt, stubs = var.stubs, ids = id.vars, keep.all = keep.all,
               value.name = NULL, variable.name = NULL, end_stub = end_stub)
}
NULL
