

#' Split concatenated values into their corresponding column position
#' 
#' "Expand" concatenated numeric or character values to their relevant position
#' in a \code{data.frame} or \code{data.table} or create a binary
#' representation of such data.
#' 
#' 
#' @aliases cSplit_e concat.split.expanded
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param mode Can be either \code{"binary"} (where presence of a number in a
#' given column is converted to "1") or \code{"value"} (where the value is
#' retained and not recoded to "1"). Defaults to \code{"binary"}.
#' @param type Can be either \code{"numeric"} (where the items being split are
#' positive integers (\code{> 0})) or \code{"character"} (where the items being
#' split are character strings). Defaults to \code{"numeric"}.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{strsplit} for allowing regular expressions to be
#' used.
#' @param fill Desired "fill" value. Defaults to \code{NA}.
#' @return A \code{data.frame} or a \code{data.table} depending on the source
#' input.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.list}},
#' \code{\link{concat.split.compact}}, \code{\link{concat.split.multiple}},
#' \code{\link{numMat}}, \code{\link{charMat}}
#' @name concat.split.expanded
#' @examples
#' 
#' temp <- head(concat.test)
#' cSplit_e(temp, "Likes")
#' cSplit_e(temp, 4, ";", fill = 0)
#' 
#' ## The old function name still works
#' concat.split.expanded(temp, "Likes")
#' concat.split.expanded(temp, 4, ";", fill = 0)
#' concat.split.expanded(temp, 4, ";", mode = "value", drop = TRUE)
#' concat.split.expanded(temp, "Siblings", type = "character", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
NULL





#' Split concatenated cells into a \code{list} format
#' 
#' Takes a column in a \code{data.frame} or \code{data.table} with multiple
#' values, splits the values into a \code{list}, and returns a new
#' \code{data.frame} or \code{data.table}.
#' 
#' 
#' @aliases cSplit_l concat.split.list
#' @param data The source \code{data.frame} or \code{data.table}.
#' @param split.col The variable that needs to be split (either name or index
#' position).
#' @param sep The character separating each value. Can also be a regular
#' expression.
#' @param drop Logical. Should the original variable be dropped? Defaults to
#' \code{FALSE}.
#' @param fixed Used for \code{\link{strsplit}} for allowing regular
#' expressions to be used.
#' @return A \code{data.frame} or \code{data.table} with the concatenated
#' column split and added as a \code{list}.
#' @author Ananda Mahto
#' @seealso \code{\link{concat.split}}, \code{\link{concat.split.compact}},
#' \code{\link{concat.split.expanded}}, \code{\link{concat.split.multiple}}
#' @name concat.split.list
#' @examples
#' 
#' temp <- head(concat.test)
#' str(cSplit_l(temp, "Likes"))
#' cSplit_l(temp, 4, ";")
#' 
#' ## The old function name still works
#' str(concat.split.list(temp, "Likes"))
#' concat.split.list(temp, 4, ";")
#' concat.split.list(temp, 4, ";", drop = TRUE)
#' 
#' \dontshow{rm(temp)}
#' 
NULL





#' Example dataset with concatenated cells
#' 
#' This is a sample dataset to demonstrate the different features of the
#' \code{\link{concat.split}} family of functions.
#' 
#' 
#' @name concat.test
#' @aliases concat.test concatenated
#' @docType data
#' @format A \code{data.frame} in which many columns contain concatenated cells
#' @keywords datasets
NULL





#' splitstackshape
#' 
#' Functions to split concatenated data, conveniently stack columns of
#' \code{data.frame}s, and conveniently reshape \code{data.frame}s.
#' 
#' \tabular{ll}{ Package: \tab splitstackshape\cr Type: \tab Package\cr
#' Version: \tab 1.3.7\cr Date: \tab 2014-10-10\cr License: \tab GPL-3\cr }
#' 
#' Online data collection tools like Google Forms often export
#' multiple-response questions with data concatenated in cells. The
#' \code{\link{concat.split}} family of functions conveniently splits such data
#' into separate cells. The package also includes functions to conveniently
#' \emph{stack} groups of columns and to \emph{reshape} wide data, even when
#' the data are "unbalanced"---something which \code{\link{reshape}} does not
#' handle, and which \code{\link[reshape2:melt]{melt}} and
#' \code{\link[reshape2:dcast]{dcast}} from \emph{reshape2} do not easily
#' handle.
#' 
#' @name splitstackshape-package
#' @aliases splitstackshape splitstackshape-package
#' @docType package
#' @author Ananda Mahto
#' 
#' Maintainer: Ananda Mahto <ananda@@mahto.info>
#' @keywords package
#' @import data.table
#' @examples
#' 
#' ## concat.split
#' head(cSplit(concat.test, "Likes", drop = TRUE))
#' 
#' ## Reshape
#' set.seed(1)
#' mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"),
#'                    varA.1 = sample(letters, 6),
#'                    varA.2 = sample(letters, 6),
#'                    varA.3 = sample(letters, 6),
#'                    varB.2 = sample(10, 6),
#'                    varB.3 = sample(10, 6),
#'                    varC.3 = rnorm(6))
#' mydf
#' Reshape(mydf, id.vars = c("id_1", "id_2"),
#'         var.stubs = c("varA", "varB", "varC"))
#' 
#' ## Stacked
#' Stacked(data = mydf, id.vars = c("id_1", "id_2"),
#'         var.stubs = c("varA", "varB", "varC"),
#'         sep = "\\.")
#' \dontrun{
#' ## Processing times
#' set.seed(1)
#' Nrow <- 1000000
#' Ncol <- 10
#' mybigdf <- cbind(id = 1:Nrow, as.data.frame(matrix(rnorm(Nrow*Ncol),
#'                                                    nrow=Nrow)))
#' head(mybigdf)
#' dim(mybigdf)
#' tail(mybigdf)
#' A <- names(mybigdf)
#' names(mybigdf) <- c("id", paste("varA", 1:3, sep = "_"),
#'                     paste("varB", 1:4, sep = "_"),
#'                     paste("varC", 1:3, sep = "_"))
#' system.time({
#'    O1 <- Reshape(mybigdf, id.vars = "id",
#'    var.stubs = c("varA", "varB", "varC"), sep = "_")
#'    O1 <- O1[order(O1$id, O1$time), ]
#' })
#' system.time({
#'    O2 <- merged.stack(mybigdf, id.vars="id",
#'    var.stubs=c("varA", "varB", "varC"), sep = "_")
#' })
#' system.time({
#'    O3 <- Stacked(mybigdf, id.vars="id",
#'    var.stubs=c("varA", "varB", "varC"), sep = "_")
#' })
#' DT <- data.table(mybigdf)
#' system.time({
#'    O4 <- merged.stack(DT, id.vars="id",
#'    var.stubs=c("varA", "varB", "varC"), sep = "_")
#' })
#' }
#' 
#' \dontshow{rm(mydf)}
#' 
NULL


