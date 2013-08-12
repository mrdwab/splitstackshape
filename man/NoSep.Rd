\name{NoSep}
\alias{NoSep}
\title{Split basic alphanumeric strings which have no separators}
\usage{
  NoSep(data, charfirst = TRUE)
}
\arguments{
  \item{data}{The vector of strings to be split.}

  \item{charfirst}{Is the string constructed with
  characters at the start or numbers? Defaults to
  \code{TRUE}.}
}
\value{
  A \code{data.frame} with two columns, \code{.var} and
  \code{.time_1}.
}
\description{
  Used to split strings like "Abc8" into "Abc" and "8".
}
\note{
  This is a helper function for the \code{\link{Stacked}}
  and \code{\link{Reshape}} functions.
}
\examples{
x <- paste0("Var", LETTERS[1:3], 1:3)
splitstackshape:::NoSep(x)

y <- paste0(1:3, "Var", LETTERS[1:3])
splitstackshape:::NoSep(y, charfirst = FALSE)

\dontshow{rm(x, y)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{strsplit}}
}

