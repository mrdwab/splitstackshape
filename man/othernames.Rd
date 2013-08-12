\name{othernames}
\alias{othernames}
\title{Extract all names from a \code{data.frame} other than the one listed}
\usage{
  othernames(data, toremove)
}
\arguments{
  \item{data}{The input \code{data.frame}.}

  \item{toremove}{The \code{names} you want to exclude.}
}
\value{
  A character vector of the remaining names.
}
\description{
  A convenience function for \code{setdiff(names(data),
  -some_vector_of_names-)}.
}
\examples{
mydf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
splitstackshape:::othernames(mydf, "a")

\dontshow{rm(mydf)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{setdiff}}
}

