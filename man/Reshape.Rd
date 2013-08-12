\name{Reshape}
\alias{Reshape}
\title{Reshape wide data into a semi-long form}
\usage{
  Reshape(data, id.vars, var.stubs, sep = ".", ...)
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{id.vars}{The variables that serve as unique
  identifiers.}

  \item{var.stubs}{The prefixes of the variable groups.}

  \item{sep}{The character that separates the "variable
  name" from the "times" in the wide \code{data.frame}.}

  \item{\dots}{Further arguments to \code{\link{NoSep}} in
  case the separator is of a different form.}
}
\value{
  A "long" \code{data.frame} of the reshaped data that
  retains the attributes added by base R's \code{reshape}
  function.
}
\description{
  The \code{\link{reshape}} function in base R is very
  handy when you want a semi-long (or semi-wide)
  \code{data.frame}. However, base R's \code{reshape} has
  problems is with "unbalanced" panel data, for instance
  data where one variable was measured at three points in
  time, and another only twice.
}
\details{
  This function was written to overcome that limitation of
  dealing with unbalanced data, but is also appropriate for
  basic wide-to-long reshaping tasks.

  Related functions like \code{\link{stack}} in base R and
  \code{\link[reshape2:melt]{melt}} in "reshape2" are also
  very handy when you want a "long" reshaping of data, but
  they result in a very long structuring of your data, not
  the "semi-wide" format that \code{reshape} produces.
}
\examples{
set.seed(1)
mydf <- data.frame(id_1 = 1:6, id_2 = c("A", "B"), varA.1 = sample(letters, 6),
                 varA.2 = sample(letters, 6), varA.3 = sample(letters, 6),
                 varB.2 = sample(10, 6), varB.3 = sample(10, 6),
                 varC.3 = rnorm(6))
mydf

## Note that these data are unbalanced
## reshape() will not work
\dontrun{
reshape(mydf, direction = "long", idvar=1:2, varying=3:ncol(mydf))
}

## The Reshape() function can handle such scenarios

Reshape(mydf, id.vars = c("id_1", "id_2"),
       var.stubs = c("varA", "varB", "varC"))
}
\author{
  Ananda Mahto
}
\seealso{
  \code{\link{Stacked}}, \code{\link{stack}},
  \code{\link{reshape}}, \code{\link[reshape2:melt]{melt}}
}

