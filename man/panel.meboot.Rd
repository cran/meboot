\name{panel.meboot}
 \alias{panel.meboot}
  \title{Maximum Entropy Bootstrap for Panel Time Series Data}

  \description{
    This function applies the maximum entropy bootstraped in a panel of time series data.
  }

  \usage{
    panel.meboot (x, reps, colsubj, coldata, coltimes,...)
  }

  \arguments{
    \item{x}{a matrix or data.frame object containing by columns: the individual index, an optional time index and a panel of time series data.}
    \item{reps}{number of replicates to generate for each subject in the panel.}
    \item{colsubj}{the column in \code{x} that contains the individual index.}
    \item{coldata}{the column in \code{x} that contains the data of the variable to create the ensemble.}
    \item{coltimes}{an optional argument indicating the column that contains the times at which the observations for each individual are observed.}
    \item{...}{further arguments to be passed to \code{\link{meboot}}.}
  }

  \details{
    The observations in \code{x} should be arranged by individuals. The observations for each individual must be sorted by time.

    The argument \code{colsubj} can be either a numeric or a character index indicating the individual or the time series to which each observation is related.

    Only one variable can be replicated at a time, \code{coldata} must be of length one.

    If the times at which observations are observed is provided specifying the column with the times through the argument \code{coltimes}, these times are used only to label the rows of the data.frame returned as output.
  }

  \value{
    A data.frame object of dimension: number of rows of \code{x} times number of replicates indicated in \code{reps}. The replicates for the panel of data are arranged by columns. Each replicate in each column is sorted with the same order stablished in the input \code{x}.
  }

  \seealso{\code{\link{meboot}}.}

  \examples{
    ## Ensemble for a panel of series of stock prices  
    data(ullwan)
    out <- panel.meboot(ullwan, reps=99, colsubj=2, coldata=4)
  }

\keyword{ts}


