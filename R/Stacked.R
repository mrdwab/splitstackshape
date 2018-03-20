#' Stack Columns from a Wide Form to a Long Form
#' 
#' A function to conveniently stack groups of wide columns into a long form
#' which can then be \code{\link[data.table:merge]{merge}}d together.
#' 
#' 
#' @param data The source \code{data.frame}.
#' @param id.vars The variables that serve as unique identifiers. Defaults to \code{NULL}, at which point, all names which are not identified as variable groups are used as the identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the wide \code{data.frame}. Alternatively, can be set to
#' \code{"var.stubs"} (in quotes) if you do not have a value for \code{sep}.
#' @param keep.all Logical. Should all the variables from the source
#' \code{data.frame} be kept (\code{keep.all = TRUE}) or should the resulting
#' \code{\link[data.table:data.table]{data.table}} comprise only columns for
#' the \code{id.vars}, \code{var.stubs}, and "times" (\code{keep.all = FALSE}).
#' Other variables are \emph{recycled} to appropriate length. For this to work,
#' both \code{id.vars} and \code{var.stubs} must be specified.
#' @param keyed Logical. Should the \code{Stacked} function automatically set
#' the \code{key} for the resulting \code{data.table}s. If \code{TRUE}
#' (default) the \code{key} is set to the \code{id.vars} and the "time"
#' variables that are created by \code{Stacked}.
#' @param keep.rownames Logical. Should rownames be kept when converting the input to a \code{data.table}? Defaults to \code{FALSE}.
#' @param \dots Other arguments to be passed on when \code{sep = "var.stubs"} (specifically, \code{atStart}: A logical argument to indicate whether the stubs come at the start or at the end of the variable names).
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
#' Stacked(data = mydf, var.stubs = c("varA", "varB", "varC"), sep = ".")
#' 
#' \dontshow{rm(mydf)}
#' 
#' @export Stacked
Stacked <- function(data, id.vars = NULL, var.stubs, sep, 
                    keep.all = TRUE, keyed = TRUE, 
                    keep.rownames = FALSE, ...) {
  temp1 <- vGrep(var.stubs, names(data))

  s <- sort.list(sapply(names(temp1), nchar), decreasing = TRUE)
  for (i in s) {
    matches <- temp1[[i]]
    for (j in 1:length(temp1)) {
      if (j != i && any(matches %in% temp1[[j]])) {
        temp1[[j]] <- temp1[[j]][-which(temp1[[j]] %in% matches)]
      }
    } 
  }

  temp <- Names(data, unlist(temp1))
  
  if (sep == ".") sep <- "\\."
  if (sep == "var.stubs") sep <- .collapseMe(var.stubs, ...)

  if (is.null(id.vars)) {
    id.vars <- othernames(data, temp)
  } else {
    id.vars <- Names(data, id.vars)
  }

  onames <- othernames(data, c(id.vars, temp))

  if (!isTRUE(keep.all)) onames <- NULL
  if (length(onames) == 0) onames <- NULL
  if (!isTRUE(is.data.table(data))) {
    data <- as.data.table(data, keep.rownames = keep.rownames)
  } else {
    data <- copy(data)
  }
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















#' Take a List of Stacked data.tables and Merge Them
#' 
#' A wrapper around the \code{\link{Stacked}} function to
#' \code{\link[data.table:merge]{merge}} the resulting \code{list} into a
#' single \code{data.table}.
#' 
#' 
#' @param data The input \code{data.frame}.
#' @param id.vars The columns to be used as "ID" variables. Defaults to \code{NULL}, at which point, all names which are not identified as variable groups are used as the identifiers.
#' @param var.stubs The prefixes of the variable groups.
#' @param sep The character that separates the "variable name" from the "times"
#' in the source \code{data.frame}. Alternatively, can be set to
#' \code{"var.stubs"} (in quotes) if you do not have a value for \code{sep}.
#' @param keep.all Logical. Should all the variables in the source
#' \code{data.frame} be kept (\code{keep.all = TRUE}) or only those which
#' comprise the \code{id.vars} and split data from the \code{var.stubs}
#' (\code{keep.all = FALSE}).
#' @param \dots Other arguments to be passed on to \code{\link{Stacked}} (for example, \code{keep.rownames} to retain the rownames of the input dataset, or \code{atStart}, in case \code{sep = "var.stubs"} is specified).
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
#' merged.stack(mydf, var.stubs = c("varA", "varB", "varC"), sep = ".")
#' 
#' \dontshow{rm(mydf)}
#' 
#' @export merged.stack
merged.stack <- function(data, id.vars = NULL, var.stubs, sep, keep.all = TRUE, ...) {
  temp <- Stacked(data = data, id.vars = id.vars, var.stubs = var.stubs, 
                  sep = sep, keep.all = keep.all, keyed = TRUE, ...)
  if (!is.null(dim(temp))) temp
  else Reduce(function(x, y) merge(x, y, all = TRUE), temp)
}
