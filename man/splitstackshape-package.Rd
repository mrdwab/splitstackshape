\docType{package}
\name{splitstackshape-package}
\alias{splitstackshape}
\alias{splitstackshape-package}
\title{splitstackshape}
\description{
  Functions to split concatenated data, conveniently stack
  columns of \code{data.frame}s, and conveniently reshape
  \code{data.frame}s.
}
\details{
  \tabular{ll}{ Package: \tab splitstackshape\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2013-08-11\cr
  License: \tab GPL-3\cr }

  Online data collection tools like Google Forms often
  export multiple-response questions with data concatenated
  in cells. The \code{\link{concat.split}} family of
  functions conveniently splits such data into separate
  cells. The package also includes functions to
  conveniently \emph{stack} groups of columns and to
  \emph{reshape} wide data, even when the data are
  "unbalanced"---something which \code{\link{reshape}} does
  not handle, and which \code{\link[reshape2:melt]{melt}}
  and \code{\link[reshape2:dcast]{dcast}} from
  \emph{reshape2} do not easily handle.
}
\examples{
head(concat.split(concat.test, "Likes", drop = TRUE))
}
\author{
  Ananda Mahto

  Maintainer: Ananda Mahto <ananda@mahto.info>
}
\keyword{package}

