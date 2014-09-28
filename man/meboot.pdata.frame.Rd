\name{meboot.pdata.frame}
\alias{meboot.pdata.frame}
\title{Maximum Entropy Bootstrap for Panel Time Series Data}

  \description{
    This function applies the maximum entropy bootstraped in a panel of time series data.
  }

  \usage{
    meboot.pdata.frame (x, reps=999, trim=0.10, reachbnd=TRUE,
      expand.sd=TRUE, force.clt=TRUE, scl.adjustment = FALSE, sym = FALSE, elaps=FALSE,
      colsubj, coldata, coltimes, ...)
  }

  \arguments{
    \item{x}{a \code{pdata.frame} object containing by columns: the individual index, an optional time index and a panel of time series data.}
    \item{reps}{number of replicates to generate for each subject in the panel.}
    \item{trim}{the trimming proportion.}
    \item{reachbnd}{logical. If \code{TRUE} potentially reached bounds (xmin = smallest value - trimmed mean and xmax=largest value + trimmed mean) are given when the random draw happens to be equal to 0 and 1, respectively.
    }
    \item{expand.sd}{logical. If TRUE the standard deviation in the ensemble in expanded. See \code{\link{expand.sd}}.}
    \item{force.clt}{logical.If TRUE the ensemble is forced to satisfy the central limit theorem. See \code{\link{force.clt}}.}
    \item{scl.adjustment}{logical. If \code{TRUE} scale adjustment is performed 
       to ensure that the population variance of the transformed series equals the variance of the data.}
    \item{sym}{logical. If \code{TRUE} an adjustment is peformed to ensure that the ME density is symmetric.}
    \item{elaps}{logical. If TRUE elapsed time during computations is displayed.}
    \item{colsubj}{the column in \code{x} that contains the individual index.}
    \item{coldata}{the column in \code{x} that contains the data of the variable to create the ensemble.}
    \item{coltimes}{an optional argument indicating the column that contains the times at which the observations for each individual are observed.}
    \item{...}{possible argument \code{fiv} to be passed to \code{\link{expand.sd}}.}    
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
    data("ullwan")
    out <- meboot(ullwan, reps=99, colsubj=2, coldata=4)
  }

\keyword{ts}
