\name{null.ci}
 \alias{null.ci}
  \title{Get Confidence Interval Around Specified NullZero Total}

  \description{
    Function to get two sided confidence interval around zero as the true value. Confidence interval is adjusted so that it covers the true zero (1-'level')*100 times. Symmetry is not assumed.
  }

  \usage{
    null.ci (x, level=0.95, null.value=0, type=8, ...)
  }

  \arguments{
    \item{x}{a vector of data.}
    \item{level}{confidence level.}
    \item{null.value}{a specified value of the null, e.g., 0.}
    \item{type}{type of quantile, a number between 1 and 9. See \code{\link{quantile}}.}
    \item{...}{further arguments passed to or from other methods.}
  }

  \value{
    Lower limit and upper limit of the confidence interval.
  }

  \examples{
    x <- runif(25, 0, 1)
    null.ci(x)
  }

\keyword{ts}
