\name{merged.stack}
\alias{merged.stack}
\title{Take a \code{list} of stacked \code{data.table}s and \code{merge} them}
\usage{
  merged.stack(data, id.vars, var.stubs, sep,
    keep.all = TRUE, ...)
}
\arguments{
  \item{data}{The input \code{data.frame}.}

  \item{id.vars}{The columns to be used as "ID" variables.}

  \item{var.stubs}{The prefixes of the variable groups.}

  \item{sep}{The character that separates the "variable
  name" from the "times" in the source \code{data.frame}.}

  \item{keep.all}{Logical. Should all the variables in the
  source \code{data.frame} be kept (\code{keep.all = TRUE})
  or only those which comprise the \code{id.vars} and split
  data from the \code{var.stubs} (\code{keep.all =
  FALSE}).}

  \item{\dots}{Further arguments to \code{\link{NoSep}} in
  case the separator is of a different form.}
}
\value{
  A merged \code{data.table}.
}
\description{
  A wrapper around the \code{\link{Stacked}} function to
  \code{\link[data.table:merge]{merge}} the resulting
  \code{list} into a single \code{data.table}.
}
\examples{
set.seed(1)
mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"),
                   varA.1 = sample(letters, 6),
                   varA.2 = sample(letters, 6),
                   varA.3 = sample(letters, 6),
                   varB.2 = sample(10, 6),
                   varB.3 = sample(10, 6),
                   varC.3 = rnorm(6))
mydf
merged.stack(mydf, id.vars = c("id_1", "id_2"),
             var.stubs = c("varA", "varB", "varC"),
             sep = "\\\\.")

\dontshow{rm(mydf)}
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{Stacked}}, \code{\link{Reshape}}
}

