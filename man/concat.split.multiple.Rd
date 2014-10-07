\name{concat.split.multiple}
\alias{concat.split.multiple}
\title{Split concatenated cells in a \code{data.frame} and optionally reshape the
output}
\usage{
concat.split.multiple(data, split.cols, seps = ",", direction = "wide")
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{split.cols}{A vector of columns that need to be
  split.}

  \item{seps}{A vector of the separator character used in
  each column. If all columns use the same character, you
  can enter that single character.}

  \item{direction}{The desired form of the resulting
  \code{data.frame}, either \code{'wide'} or \code{'long'}.
  Defaults to \code{'wide'}.}
}
\value{
A \code{data.frame}. If \code{direction = "long"}, a
\code{data.frame} with additional attributes created by the
\code{\link{reshape}} function in base R.
}
\description{
This is an extended version of the
\code{\link{concat.split.compact}} function that allows the
user to split multiple columns at once and optionally use
the \code{\link{Reshape}} function to convert the
\code{data.frame} into a "long" format.
}
\examples{
temp <- head(concat.test)
concat.split.multiple(temp, split.cols = c("Likes", "Hates", "Siblings"),
                      seps = c(",", ";", ","))
concat.split.multiple(temp, split.cols = c("Likes", "Siblings"),
                      seps = ",", direction = "long")
\dontshow{rm(temp)}
}
\author{
Ananda Mahto
}
\seealso{
\code{\link{concat.split}},
\code{\link{concat.split.compact}},
\code{\link{concat.split.expanded}},
\code{\link{concat.split.multiple}}, \code{\link{Reshape}}
}

