# nocov start
.onAttach <- function(libname, pkgname) {
  M <- sprintf("This is 'splitstackshape' version %s.", 
               packageVersion("splitstackshape"))
  if (packageVersion("data.table") < "1.10.5") {
    M <- c(M, "'splitstackshape' works best with 'data.table' >= 1.10.5.")
  }
  if (!(requireNamespace("stringi", quietly = TRUE))) {
    M <- c(M, "The 'stringi' package is also strongly recommended.")
  }
  packageStartupMessage(paste(M, collapse = "\n"))
}
# nocov end

#' Example Dataset with Concatenated Cells
#' 
#' This is a sample dataset to demonstrate the different features of the
#' [concat.split()] family of functions.
#' 
#' 
#' @name concat.test
#' @aliases concat.test concatenated
#' @docType data
#' @format A `data.frame` in which many columns contain concatenated cells
#' @keywords datasets
NULL





#' splitstackshape
#' 
#' Stack and Reshape Datasets After Splitting Concatenated Values
#' 
#' \tabular{ll}{ Package: \tab splitstackshape\cr Type: \tab Package\cr
#' Version: \tab 2.0.0000-pre_release\cr Date: \tab 2018-03-30\cr License: \tab GPL-3\cr }
#' 
#' Online data collection tools like Google Forms often export multiple-response 
#' questions with data concatenated in cells. The [concat.split()] family of 
#' functions splits such data into separate cells. The package also includes 
#' functions to *stack* groups of columns and to *reshape* wide data, even when
#' the data are "unbalanced"---something which [stats::reshape()] does not
#' handle, and which [reshape2::melt()] and [reshape2::dcast()] from *reshape2* 
#' do not easily handle.
#' 
#' @name splitstackshape-package
#' @aliases splitstackshape splitstackshape-package
#' @docType package
#' @author Ananda Mahto
#' 
#' Maintainer: Ananda Mahto <ananda@@mahto.info>
#' @keywords package
#' @import data.table
#' @importFrom stats reshape setNames
#' @importFrom utils count.fields read.table
#' @examples
#' 
#' ## concat.split
#' head(cSplit(concat.test, "Likes", drop = TRUE))
#' 
#' ## Stacked
#' DF <- data.frame(id_1 = 1:3, id_2 = c("A", "B", "A"),
#'                  varA.1 = c("a", "b", "c"), varA.2 = c("x", "y", "z"), 
#'                  varA.3 = c("q", "r", "s"), varB.2 = c(10, 20, 30),
#'                  varB.3 = c(11, 22, 33), varC.3 = c(-1.23, .992, -.351))
#' 
#' Stacked(DF, id.vars = c("id_1", "id_2"), var.stubs = c("varA", "varB", "varC"))
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


