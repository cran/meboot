\name{meboot.part}
\alias{meboot.part}
\title{meboot Internal Function}

\description{
Internal function.
}

\usage{
  meboot.part (x, n, z, xmin, xmax, desintxb, reachbnd)
}

\arguments{
  \item{x}{vector of data, \code{ts} object or \code{pdata.frame} object.}
  \item{n}{length of \code{x}.}
  \item{z}{class intervals limits.}
  \item{xmin}{lower limit in the left tail.}
  \item{xmax}{upper limit in the left tail}
  \item{desintxb}{desired inteval means.}
  \item{reachbnd}{logical. If TRUE potentially reached bounds (xmin = smallest value - trimmed mean and xmax=largest value + trimmed mean) are given when the random draw happens to be equal to 0 and 1, respectively.}
}

\value{
A vector of resampled data.
}

\seealso{\code{\link{meboot}}.}

\keyword{ts}
