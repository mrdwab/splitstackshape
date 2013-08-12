\name{concat.split.compact}
\alias{concat.split.compact}
\title{Split concatenated cells in a \code{data.frame} into a condensed format}
\usage{
  concat.split.compact(data, split.col, sep = ",",
    drop = FALSE, fixed = NULL)
}
\arguments{
  \item{data}{The source \code{data.frame}}

  \item{split.col}{The variable that needs to be split
  (either name or index position).}

  \item{sep}{The character separating each value.}

  \item{drop}{Logical. Should the original variable be
  dropped? Defaults to \code{FALSE}.}

  \item{fixed}{An unused dummy argument to make the
  function compatible with
  \code{\link{concat.split.expanded}}.}
}
\value{
  A \code{data.frame}
}
\description{
  The default splitting method for
  \code{\link{concat.split}}. Uses
  \code{\link{read.concat}} to do most of the processing.
}
\examples{
temp <- head(concat.test)
concat.split.compact(temp, "Likes")
concat.split.compact(temp, 4, ";")
concat.split.compact(temp, "Siblings", drop = TRUE)

\dontshow{rm(temp)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{read.concat}}, \code{\link{concat.split}},
  \code{\link{concat.split.list}},
  \code{\link{concat.split.expanded}},
  \code{\link{concat.split.multiple}}
}

