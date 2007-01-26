\name{slider.mts}
 \alias{slider.mts}
  \title{Slider for Selecting a Single Time Series to Plot from a 'mts' Object}

  \description{
    This function displays single plots from a 'mts' object. The time series to plot is selected through a slider scaled from 1 to the number total of times series in the object.
  }

  \usage{
    slider.mts (x,...)
  }

  \arguments{
    \item{x}{an object of class \code{mts}.}
    \item{...}{further arguments passed to \code{\link{plot}}.}
  }

  \details{
    Requires the \code{tcltk} package.
  }

  \seealso{\code{\link{meboot}}.}

  \examples{
    ## Ensemble for the AirPassenger time series data
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100)
    slider.mts(ts.union(AirPassengers, out$ensemble), 
      ylim=range(out$ensemble))
  }

\keyword{ts}


