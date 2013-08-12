
#' Example dataset with concatenated cells
#' 
#' This is a sample dataset to demonstrate the different features of the
#' \code{\link{concat.split}} family of functions.
#' 
#' 
#' @name concat.test
#' @aliases concatenated concat.test
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
#' Version: \tab 1.0\cr Date: \tab 2013-08-12\cr License: \tab GPL-3\cr }
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
#' @examples
#' 
#' ## concat.split
#' head(concat.split(concat.test, "Likes", drop = TRUE))
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
#'         
#' \dontshow{rm(mydf)}
#'   
NULL



