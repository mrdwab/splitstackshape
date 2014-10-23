#' Example Dataset with Concatenated Cells
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
#' Stack and Reshape Datasets After Splitting Concatenated Values
#' 
#' \tabular{ll}{ Package: \tab splitstackshape\cr Type: \tab Package\cr
#' Version: \tab 1.4.2\cr Date: \tab 2014-10-23\cr License: \tab GPL-3\cr }
#' 
#' Online data collection tools like Google Forms often export
#' multiple-response questions with data concatenated in cells. The
#' \code{\link{concat.split}} family of functions splits such data
#' into separate cells. The package also includes functions to 
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
#'         sep = ".")
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


