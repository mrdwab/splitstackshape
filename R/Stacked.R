#' Stack Columns from a Wide Form to a Long Form
#' 
#' A function to conveniently stack groups of wide columns into a long form.
#' 
#' @param indt The source `data.table`.
#' @param id.vars The variables that serve as unique identifiers. Defaults to 
#' `NULL`, at which point, all names which are not identified as variable groups 
#' are used as the identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param keep.all Logical. Should all the variables from the source `data.table`
#' be kept (`keep.all = TRUE`) or should the resulting `data.table` comprise 
#' only columns for the `id.vars`, `var.stubs`, and "times" (`keep.all = FALSE`).
#' Other variables are *recycled* to appropriate length. For this to work, both 
#' `id.vars` and `var.stubs` must be specified.
#' @param keyed Logical. Should the function automatically set the 
#' [data.table::key()] for the resulting `data.table`s. If `TRUE` (default) the 
#' `key` is set to the `id.vars` and the "time" variables that are created.
#' @param keep.rownames Logical. Should rownames be kept when converting the 
#' input to a `data.table`? Defaults to `FALSE`.
#' @param end_stub Logical. Are the `var.stubs` found at the start (eg: 
#' `"Var_1"`) or at the end (eg: `"Q1_Var"`)? Defaults to `TRUE`.
#' @return A `list` of `data.table`s with one `data.table` for each "var.stub". 
#' The `key` is set to the `id.vars` and `.time_#` vars.
#' @note This is the function internally called by [merged.stack()].
#' @author Ananda Mahto
#' @seealso [utils::stack()], [reshape2::melt()] from "reshape2".
#' @examples
#' 
#' DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"),
#'                  varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
#'                  varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
#'                  varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
#' Stacked(DF, var.stubs = c("varA", "varB", "varC"))
#' 
#' @export Stacked
Stacked <- function(indt, id.vars = NULL, var.stubs, 
                    keep.all = TRUE, keyed = TRUE, 
                    keep.rownames = FALSE, end_stub = FALSE) {
  
  indt <- setDT(copy(indt), keep.rownames)
  check <- all_names(names(indt), var.stubs, end_stub)
  
  id.vars <- if (is.null(id.vars)) check[["id_names"]] else id.vars
  onames <- setdiff(names(indt), c(check[["stub_names"]], id.vars))
  if (!keep.all | length(onames) == 0L) onames <- NULL
  
  ZZ <- vector("list", length(var.stubs))
  names(ZZ) <- var.stubs
  
  for (i in seq_along(var.stubs)) {
    COLS <- check[["stub_list"]][[i]]
    .time <- gsub("^[[:punct:]]|[[:punct:]]$", "", sub(var.stubs[i], "", COLS))
    ZZ[[i]] <- melt(
      indt[, c(id.vars, COLS, if (keep.all) onames else NULL), with = FALSE],
      measure = COLS, value.name = var.stubs[i])
    setkeyv(ZZ[[i]], id.vars)
    ZZ[[i]] <- set(ZZ[[i]], j = "variable", value = .time)
    if (keyed) {
      setkeyv(ZZ[[i]], c(id.vars, "variable"))
      setcolorder(ZZ[[i]], c(key(ZZ[[i]]), var.stubs[i], onames))
    }
  }
  
  if (length(ZZ) == 1L) ZZ[[1L]] else ZZ
}
NULL
