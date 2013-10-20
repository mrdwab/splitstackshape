\name{binaryMat}
\alias{binaryMat}
\title{Create a binary matrix from a list of values}
\usage{
  binaryMat(listOfValues, fill = NA)
}
\arguments{
  \item{listOfValues}{A \code{list} of input values to be
  inserted in a matrix.}

  \item{fill}{The initializing fill value for the empty
  matrix.}
}
\value{
  A \code{matrix}.
}
\description{
  Create a binary matrix from a list of values
}
\details{
  This is primarily a helper function for the
  \code{\link{concat.split}} function when creating the
  "expanded" structure. The input is anticipated to be a
  \code{list} of values obtained using
  \code{\link{strsplit}}.
}
\examples{
invec <- c("1,2,4,5,6", "1,2,4,5,6", "1,2,4,5,6",
           "1,2,4,5,6", "1,2,5,6", "1,2,5,6")
A <- strsplit(invec, ",")
splitstackshape:::binaryMat(A)
splitstackshape:::binaryMat(A, "ZZZ")

\dontshow{rm(invec, A)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{strsplit}, \code{\link{valueMat}},
  \code{\link{charBinaryMat}}
}

