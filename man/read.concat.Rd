\name{read.concat}
\alias{read.concat}
\title{Read concatenated character vectors into a \code{data.frame}}
\usage{
  read.concat(data, col.prefix, sep)
}
\arguments{
  \item{data}{The input data.}

  \item{col.prefix}{The desired column prefix for the
  output \code{data.frame}.}

  \item{sep}{The character that acts as a delimiter.}
}
\value{
  A \code{data.frame}
}
\description{
  A helper function for the
  \code{\link{concat.split.compact}} function.
}
\examples{
vec <- c("a,b", "c,d,e", "f, g", "h, i, j,k")
splitstackshape:::read.concat(vec, "var", ",")

## More than 5 lines the same
## `read.table` would fail with this
vec <- c("12,51,34,17", "84,28,17,10", "11,43,28,15",
"80,26,17,91", "10,41,25,13", "97,35,23,12,13")
splitstackshape:::read.concat(vec, "var", ",")

\dontshow{rm(vec)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{read.table}
}

