\name{concat.split.expanded}
\alias{concat.split.expanded}
\title{Split numeric concatenated values into their corresponding column position}
\usage{
  concat.split.expanded(data, split.col, sep = ",",
    mode = NULL, drop = FALSE, fixed = FALSE, fill = NA)
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{split.col}{The variable that needs to be split
  (either name or index position).}

  \item{sep}{The character separating each value. Can also
  be a regular expression.}

  \item{mode}{Can be either \code{"binary"} (where presence
  of a number in a given column is converted to "1") or
  \code{"value"} (where the value is retained and not
  recoded to "1").}

  \item{drop}{Logical. Should the original variable be
  dropped? Defaults to \code{FALSE}.}

  \item{fixed}{Used for \code{strsplit} for allowing
  regular expressions to be used.}

  \item{fill}{Desired "fill" value. Defaults to \code{NA}.}
}
\value{
  A \code{data.frame}
}
\description{
  "Expand" concatenated numeric values to their relevant
  position in a \code{data.frame}.
}
\note{
  When \code{mode = "binary"} is selected, the function
  calls \code{\link{binaryMat}} to expand the values. When
  \code{mode = "value"} is selected, the function calls
  \code{\link{valueMat}}.
}
\examples{
temp <- head(concat.test)
concat.split.expanded(temp, "Likes")
concat.split.expanded(temp, 4, ";")
concat.split.expanded(temp, 4, ";", mode = "value", drop = TRUE)

## Note the warning
concat.split.expanded(temp, "Siblings", drop = TRUE)

\dontshow{rm(temp)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{concat.split}},
  \code{\link{concat.split.list}},
  \code{\link{concat.split.compact}},
  \code{\link{concat.split.multiple}},
  \code{\link{binaryMat}}, \code{\link{valueMat}}
}

