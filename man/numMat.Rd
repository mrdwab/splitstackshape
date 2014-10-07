\name{numMat}
\alias{numMat}
\title{Create a numeric matrix from a list of values}
\usage{
numMat(listOfValues, fill = NA, mode = "binary")
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
Create a numeric matrix from a list of values
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
splitstackshape:::numMat(A)
splitstackshape:::numMat(A, fill = 0)
splitstackshape:::numMat(A, mode = "value")

\dontshow{rm(invec, A)}
}
\author{
Ananda Mahto
}
\seealso{
\code{strsplit}, \code{\link{charMat}}
}

