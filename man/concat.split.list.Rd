\name{concat.split.list}
\alias{concat.split.list}
\title{Split concatenated cells in a \code{data.frame} into a \code{list} format}
\usage{
  concat.split.list(data, split.col, sep = ",",
    drop = FALSE, fixed = FALSE)
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{split.col}{The variable that needs to be split
  (either name or index position).}

  \item{sep}{The character separating each value. Can also
  be a regular expression.}

  \item{drop}{Logical. Should the original variable be
  dropped? Defaults to \code{FALSE}.}

  \item{fixed}{Used for \code{\link{strsplit}} for allowing
  regular expressions to be used.}
}
\value{
  A \code{data.frame} with the concatenated column split
  and added as a \code{list}.
}
\description{
  Takes a column in a \code{data.frame} with multiple
  values, splits the values into a \code{list}, and returns
  a new \code{data.frame}.
}
\examples{
temp <- head(concat.test)
str(concat.split.list(temp, "Likes"))
concat.split.list(temp, 4, ";")
concat.split.list(temp, 4, ";", drop = TRUE)

\dontshow{rm(temp)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{concat.split}},
  \code{\link{concat.split.compact}},
  \code{\link{concat.split.expanded}},
  \code{\link{concat.split.multiple}}
}

