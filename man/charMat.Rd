\name{charMat}
\alias{charMat}
\title{Create a binary matrix from a list of character values}
\usage{
charMat(listOfValues, fill = NA, mode = "binary")
}
\arguments{
  \item{listOfValues}{A \code{list} of input values to be
  inserted in a matrix.}

  \item{fill}{The initializing fill value for the empty
  matrix.}

  \item{mode}{Either \code{"binary"} or \code{"value"}.
  Defaults to \code{"binary"}.}
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
splitstackshape:::charMat(A)
splitstackshape:::charMat(A, 0)
splitstackshape:::charMat(A, mode = "value")

\dontshow{rm(invec, A)}
}
\author{
Ananda Mahto
}
\seealso{
\code{strsplit}, \code{\link{numMat}}
}

