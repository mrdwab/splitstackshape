#' Converts a Distance Matrix to a data.table
#' 
#' Converts a distance matrix to a `data.table`.
#' 
#' @param inDist The input distance object.
#' @return A `data.table`.
#' @author Ananda Mahto
#' @references <http://stackoverflow.com/q/23474729/1270695>
#' @examples
#' 
#' dd <- as.dist((1 - cor(USJudgeRatings)[1:5, 1:5])/2)
#' dist2dt(dd)
#' 
#' @export
dist2dt <- function(inDist) {
  if (class(inDist) != "dist") stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels"))) sequence(A) else attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag"))) attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper"))) attr(inDist, "Upper") <- FALSE
  data.table(
    row = B[unlist(lapply(sequence(A)[-1L], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1L):1L),
    value = as.vector(inDist))
}
NULL