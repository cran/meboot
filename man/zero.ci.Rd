\name{zero.ci}
 \alias{zero.ci}
  \title{Get Confidence Interval Around Zero}

  \description{
    Function to get two sided confidence interval around zero as the true value. Confidence interval is adjusted so that it covers the true zero (1-'confl')*100 times. Symmetry is not assumed.
  }

  \usage{
    zero.ci (x, confl=0.05)
  }

  \arguments{
    \item{x}{a vector of data.}
    \item{confl}{confidence level.}
  }

  \value{
    \item{bnlo}{count of number of items below lower limit.}
    \item{bnup}{count of number of items above upper limit.}
    \item{lolim}{lower limit of the confidence interval.}
    \item{uplim}{upper limit of the confidence interval.}
  }

  \examples{
    x <- runif(25, 0, 1)
    zero.ci(x)
  }

\keyword{ts}


