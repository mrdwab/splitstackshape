\name{charBinaryMat}
\alias{charBinaryMat}
\title{Create a binary matrix from a list of character values}
\usage{
  charBinaryMat(listOfValues, fill = NA)
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
  Create a binary matrix from a list of character values
}
\details{
  This is primarily a helper function for the
  \code{\link{concat.split}} function when creating the
  "expanded" structure. The input is anticipated to be a
  \code{list} of values obtained using
  \code{\link{strsplit}}.
}
\examples{
invec <- c("rock,electro","electro","rock,jazz")
A <- strsplit(invec, ",")
splitstackshape:::charBinaryMat(A)
splitstackshape:::charBinaryMat(A, 0)

\dontshow{rm(invec, A)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{strsplit}, \code{\link{binaryMat}},
  \code{\link{valueMat}}
}

