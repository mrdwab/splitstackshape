#' Reshape Wide Data Into a Semi-long Form
#' 
#' The [stats::reshape()] function in base R is very handy when you want a
#' semi-long (or semi-wide) `data.frame`. However, base R's `reshape` has 
#' problems is with "unbalanced" panel data, for instance data where one 
#' variable was measured at three points in time, and another only twice.
#' 
#' This function was written to overcome that limitation of dealing with
#' unbalanced data, but is also appropriate for basic wide-to-long reshaping
#' tasks.
#' 
#' Related functions like [utils::stack()] in base R and [reshape2::melt()] in 
#' "reshape2" are also very handy when you want a "long" reshaping of data, but 
#' they result in a very long structuring of your data, not the "semi-wide" 
#' format that `reshape` produces. [data.table::melt()] can produce output like
#' `reshape`, but it also expects an equal number of measurements for each
#' variable.
#' 
#' @param data The source `data.frame`.
#' @param id.vars The variables that serve as unique identifiers. Defaults to 
#' `NULL`, at which point, all names which are not identified as variable groups 
#' are used as the identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the wide `data.frame`.
#' @param rm.rownames Ignored as `data.table`s do not have rownames anyway.
#' @param \dots Further arguments to [NoSep()] in case the separator is of a 
#' different form.
#' @return A "long" `data.table` of the reshaped data that retains the
#' attributes added by base R's `reshape` function.
#' @author Ananda Mahto
#' @seealso [Stacked()], [utils::stack()], [stats::reshape()], 
#' [reshape2::melt()], [data.table::melt()]
#' @examples
#' 
#' set.seed(1)
#' mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"), varA.1 = sample(letters, 6),
#'                  varA.2 = sample(letters, 6), varA.3 = sample(letters, 6),
#'                  varB.2 = sample(10, 6), varB.3 = sample(10, 6),
#'                  varC.3 = rnorm(6))
#' mydf
#' 
#' ## Note that these data are unbalanced
#' ## reshape() will not work
#' \dontrun{
#' reshape(mydf, direction = "long", idvar=1:2, varying=3:ncol(mydf))
#' }
#' 
#' ## The Reshape() function can handle such scenarios
#' 
#' Reshape(mydf, id.vars = c("id_1", "id_2"),
#'        var.stubs = c("varA", "varB", "varC"))
#' 
#' @export Reshape
Reshape <- function(data, id.vars = NULL, var.stubs, sep = ".", rm.rownames, ...) {
  if (sep == ".") sep <- "\\."
  temp <- Names(data, unlist(vGrep(var.stubs, names(data), value = TRUE)))
  
  if (is.null(id.vars)) id.vars <- othernames(data, temp)
  
  data <- getanID(data, id.vars)
  if (isTRUE(".id" %in% names(data))) id.vars = c(Names(data, id.vars), ".id")
  
  if (sep == "NoSep") {
    x <- NoSep(temp, ...)
  } else {
    x <- as.data.frame(do.call(rbind, strsplit(temp, split = sep)))
    names(x) <- c(".var", paste(".time", 1:(length(x)-1), sep = "_"))
  }
  
  xS <- split(x[, ".time_1"], x[, ".var"])
  xL <- unique(unlist(xS))
  
  if (isTRUE(all(sapply(xS, function(x) all(xL %in% x))))) {
    out <- data
  } else {
    newVars <- unlist(lapply(names(xS), function(y) {
      temp <- xL[!xL %in% xS[[y]]]
      if (length(temp) == 0) {
        temp <- NULL
      } else {
        paste(y, temp, sep = if (sep == "\\.") "." else sep)
      }
    }))
    myMat <- setNames(data.frame(
      matrix(NA, nrow = nrow(data), ncol = length(newVars))), newVars)
    out <- cbind(data, myMat)
  }
  out <- reshape(out, direction = "long", idvar = id.vars,
                 varying = lapply(vGrep(var.stubs, names(out), value = TRUE), sort),
                 sep = sep, v.names = var.stubs)
  
  if (!missing(rm.rownames)) {
    warning("argument rm.rownames is deprecated.", call. = FALSE)
  }

  out

}
NULL
