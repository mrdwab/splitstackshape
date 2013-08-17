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
Stacked <- function(data, id.vars, var.stubs, sep, keep.all = TRUE, ...) {
  vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)
  temp1 <- vGrep(var.stubs, names(data))
  temp2 <- suppressWarnings(lapply(temp1, function(y) {
    if (any(sapply(data[y], is.factor))) t1 <- stack(FacsToChars(data[y]))
    else t1 <- stack(data[y])
    if (sep == "NoSep") {
      splitcols <- NoSep(t1$ind, ...)
    } else {
      splitcols <- do.call(rbind.data.frame, 
              strsplit(as.character(t1$ind), split = sep))
      names(splitcols) <- 
        c(".var", paste(".time", 1:(length(splitcols)-1), sep = "_"))
    }
    names(t1)[1] <- as.character(splitcols$.var[1])
    t1$ind <- NULL
    timevars <- grep(".time_", names(splitcols), value = TRUE)
    A <- setdiff(seq_along(data), c(match(names(data[id.vars]), names(data)),
                                    unlist(temp1, use.names = FALSE)))
    if (isTRUE(keep.all) & length(A) > 0) {
      t2 <- data.table(
        cbind(data[id.vars], splitcols[othernames(splitcols, ".var")], t1, data[A]), 
        key = c(Names(data, id.vars), timevars))
    } else {
      t2 <- data.table(
        cbind(data[id.vars], splitcols[othernames(splitcols, ".var")], t1), 
        key = c(Names(data, id.vars), timevars))
    }
    t2
  }))
  if (length(temp2) == 1) temp2[[1]]
  else temp2
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
#' @note This is not a particularly fast function. It was the original concept
#' for \code{\link{Reshape}}, but \code{Reshape} will generally be \emph{much}
#' faster because neither \code{\link{Reduce}} nor \code{merge} are
#' particularly fast.
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
                  sep = sep, keep.all = keep.all, ...)
  if (length(temp) == 1) temp[[1]]
  else Reduce(function(x, y) merge(x, y, all = TRUE), temp)
}
