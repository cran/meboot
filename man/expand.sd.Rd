\name{expand.sd}
 \alias{expand.sd}
  \title{Expand the Standard Deviation of Resamples}

  \description{
    This function expands the standard deviation of the simulated data. Expansion is needed since some of the ratios of the actual standard deviation to that of the original data are lower than 1 due to attenuation.
  }

  \usage{
    expand.sd (x, ensemble, fiv=5)
  }

  \arguments{
    \item{x}{a vector of data or a time series object.}
    \item{ensemble}{a matrix or \code{mts} object containing resamples of the original data \code{x}.}
    \item{fiv}{reference value for the upper limit of a uniform distribution used in expansion. For example, if equal to 5 the standard deviation of each resample is expanded through a value from a uniform random distribution with lower limit equal to 1 and upper limit equal to 1+(5/100)=1.05.}
  }

  \value{
    Resamples (by columns) with expanded standard deviations.
  }

  \examples{    
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, reachbnd=FALSE, elaps=TRUE) 
    exp.ens <- expand.sd(x=AirPassengers, out$ensemble)
  }

\keyword{ts}


