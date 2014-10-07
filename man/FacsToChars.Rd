\name{FacsToChars}
\alias{FacsToChars}
\title{Convert all \code{factor} columns to \code{character} columns in a
\code{data.frame}}
\usage{
FacsToChars(mydf)
}
\arguments{
  \item{mydf}{The name of your \code{data.frame}}
}
\description{
Sometimes, we forget to use the \code{stringsAsFactors}
argument when using \code{\link{read.table}} and related
functions. By default, R converts character columns to
factors. Instead of re-reading the data, the
\code{\link{FacsToChars}} function will identify which
columns are currently factors, and convert them all to
characters.
}
\examples{
## Some example data
dat <- data.frame(title = c("title1", "title2", "title3"),
         author = c("author1", "author2", "author3"),
         customerID = c(1, 2, 1))

str(dat) # current structure
dat2 <- splitstackshape:::FacsToChars(dat)
str(dat2) # Your new object
str(dat)  # Original object is unaffected

\dontshow{rm(dat, dat2, dat_copy)}
}
\author{
Ananda Mahto
}
\seealso{
\code{\link{read.table}}
}

