\name{Stacked}
\alias{Stacked}
\title{\code{stack} columns from a wide form to a long form}
\usage{
Stacked(data, id.vars, var.stubs, sep, keep.all = TRUE, keyed = TRUE, ...)
}
\arguments{
  \item{data}{The source \code{data.frame}.}

  \item{id.vars}{The variables that serve as unique
  identifiers.}

  \item{var.stubs}{The prefixes of the variable groups.}

  \item{sep}{The character that separates the "variable
  name" from the "times" in the wide \code{data.frame}.}

  \item{keep.all}{Logical. Should all the variables from
  the source \code{data.frame} be kept (\code{keep.all =
  TRUE}) or should the resulting
  \code{\link[data.table:data.table]{data.table}} comprise
  only columns for the \code{id.vars}, \code{var.stubs},
  and "times" (\code{keep.all = FALSE}). Other variables
  are \emph{recycled} to appropriate length.}

  \item{keyed}{Logical. Should the \code{Stacked} function
  automatically set the \code{key} for the resulting
  \code{data.table}s. If \code{TRUE} (default) the
  \code{key} is set to the \code{id.vars} and the "time"
  variables that are created by \code{Stacked}.}

  \item{\dots}{Further arguments to \code{\link{NoSep}} in
  case the separator is of a different form.}
}
\value{
A \code{list} of \code{data.table}s with one
\code{data.table} for each "var.stub". The
\code{\link[data.table:key]{key}} is set to the
\code{id.vars} and \code{.time_#} vars.
}
\description{
A function to conveniently stack groups of wide columns
into a long form which can then be
\code{\link[data.table:merge]{merge}}d together.
}
\note{
This is the function internally called by
\code{\link{merged.stack}}.
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
Stacked(data = mydf, id.vars = c("id_1", "id_2"),
        var.stubs = c("varA", "varB", "varC"),
        sep = "\\\\.")

\dontshow{rm(mydf)}
}
\author{
Ananda Mahto
}
\seealso{
\code{\link{stack}}, \code{\link[reshape2:melt]{melt}} from
"reshape2".
}

