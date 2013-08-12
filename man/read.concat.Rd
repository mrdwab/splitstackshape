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

\dontshow{rm(vec)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{read.table}
}

