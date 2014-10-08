#' \code{stack} columns from a wide form to a long form
#' 
#' A function to conveniently stack groups of wide columns into a long form
#' which can then be \code{\link[data.table:merge]{merge}}d together.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param id.vars The variables that serve as unique identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the wide \code{data.frame}.
#' @param keep.all Logical. Should all the variables from the source
#' \code{data.frame} be kept (\code{keep.all = TRUE}) or should the resulting
#' \code{\link[data.table:data.table]{data.table}} comprise only columns for
#' the \code{id.vars}, \code{var.stubs}, and "times" (\code{keep.all = FALSE}).
#' Other variables are \emph{recycled} to appropriate length.
#' @param keyed Logical. Should the \code{Stacked} function automatically set
#' the \code{key} for the resulting \code{data.table}s. If \code{TRUE}
#' (default) the \code{key} is set to the \code{id.vars} and the "time"
#' variables that are created by \code{Stacked}.
#' @param \dots Further arguments to \code{\link{NoSep}} in case the separator
#' is of a different form.
#' @return A \code{list} of \code{data.table}s with one \code{data.table} for
#' each "var.stub". The \code{\link[data.table:key]{key}} is set to the
#' \code{id.vars} and \code{.time_#} vars.
#' @note This is the function internally called by \code{\link{merged.stack}}.
#' @author Ananda Mahto
#' @seealso \code{\link{stack}}, \code{\link[reshape2:melt]{melt}} from
#' "reshape2".
#' @examples
#' 
#' set.seed(1)
#' mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"),
#'                    varA.1 = sample(letters, 6),
#'                    varA.2 = sample(letters, 6),
#'                    varA.3 = sample(letters, 6),
#'                    varB.2 = sample(10, 6),
#'                    varB.3 = sample(10, 6),
#'                    varC.3 = rnorm(6))
#' mydf
#' Stacked(data = mydf, id.vars = c("id_1", "id_2"),
#'         var.stubs = c("varA", "varB", "varC"),
#'         sep = "\\.")
#' 
#' \dontshow{rm(mydf)}
#' 
#' @export Stacked
Stacked <- function(data, id.vars, var.stubs, sep, 
                    keep.all = TRUE, keyed = TRUE, ...) {
  vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
  temp1 <- vGrep(var.stubs, names(data))
  if (is.numeric(id.vars)) id.vars <- names(data)[id.vars]
  onames <- setdiff(
    names(data), 
    c(id.vars, names(data)[unlist(temp1, use.names=FALSE)]))
  if (!isTRUE(keep.all)) onames <- NULL
  if (length(onames) == 0) onames <- NULL
  if (!isTRUE(is.data.table(data))) data <- data.table(data)
  ZZ <- vector("list", length(var.stubs))
  names(ZZ) <- var.stubs
  .SD <- .N <- count <- a <- NULL
  TimeCols <- 
    lapply(seq_along(var.stubs), function(i) {
      x <- do.call(rbind, strsplit(names(data)[temp1[[i]]], sep))
      if (ncol(x) == 1L) {
        colnames(x) <- ".time_1"
        x
      } else {
        colnames(x) <- c(
          ".var", paste(".time", 1:(ncol(x)-1), sep = "_"))
        x[, -1, drop = FALSE]
      } 
    })

  for (i in seq_along(var.stubs)) {
    ZZ[[i]] <-  cbind(
      data[, c(id.vars, onames), with = FALSE],
      data[, list(.values = unlist(.SD, use.names=FALSE)), 
           .SDcols = temp1[[i]]])
    setnames(ZZ[[i]], ".values", var.stubs[[i]])
    setkeyv(ZZ[[i]], id.vars)
    ZZ[[i]] <- cbind(ZZ[[i]], TimeCols[[i]])
    if (isTRUE(keyed)) {
      setkeyv(ZZ[[i]], c(key(ZZ[[i]]), colnames(TimeCols[[i]])))
      setcolorder(ZZ[[i]], c(key(ZZ[[i]]), var.stubs[[i]], onames))
    } 
  }

  if (length(ZZ) == 1) ZZ[[1]]
  else ZZ
}
NULL











#' Take a \code{list} of stacked \code{data.table}s and \code{merge} them
#' 
#' A wrapper around the \code{\link{Stacked}} function to
#' \code{\link[data.table:merge]{merge}} the resulting \code{list} into a
#' single \code{data.table}.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param id.vars The columns to be used as "ID" variables.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the source \code{data.frame}.
#' @param keep.all Logical. Should all the variables in the source
#' \code{data.frame} be kept (\code{keep.all = TRUE}) or only those which
#' comprise the \code{id.vars} and split data from the \code{var.stubs}
#' (\code{keep.all = FALSE}).
#' @param \dots Further arguments to \code{\link{NoSep}} in case the separator
#' is of a different form.
#' @return A merged \code{data.table}.
#' @note The \code{keyed} argument to \code{\link{Stacked}} has been hard-
#' coded to \code{TRUE} to make \code{merge} work.
#' @author Ananda Mahto
#' @seealso \code{\link{Stacked}}, \code{\link{Reshape}}
#' @examples
#' 
#' set.seed(1)
#' mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"),
#'                    varA.1 = sample(letters, 6),
#'                    varA.2 = sample(letters, 6),
#'                    varA.3 = sample(letters, 6),
#'                    varB.2 = sample(10, 6),
#'                    varB.3 = sample(10, 6),
#'                    varC.3 = rnorm(6))
#' mydf
#' merged.stack(mydf, id.vars = c("id_1", "id_2"),
#'              var.stubs = c("varA", "varB", "varC"),
#'              sep = "\\.")
#' 
#' \dontshow{rm(mydf)}
#' 
#' @export merged.stack
merged.stack <- function(data, id.vars, var.stubs, sep, keep.all = TRUE, ...) {
  temp <- Stacked(data = data, id.vars = id.vars, var.stubs = var.stubs,
                  sep = sep, keep.all = keep.all, keyed = TRUE, ...)
  if (length(temp) == 1) temp[[1]]
  else Reduce(function(x, y) merge(x, y, all = TRUE), temp)
}
