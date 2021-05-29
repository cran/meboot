\name{meboot}
 \alias{meboot}
  \title{Generate Maximum Entropy Bootstrapped Time Series Ensemble}

  \description{
    Generates maximum entropy bootstrap replicates for dependent data. (See details.)
  }

  \usage{
    meboot (x, reps=999, trim=list(trim=0.10, xmin=NULL, xmax=NULL), reachbnd=TRUE,
  expand.sd=TRUE, force.clt=TRUE, scl.adjustment = FALSE, sym = FALSE,
  elaps=FALSE, colsubj, coldata, coltimes, ...)
  }

  \arguments{
    \item{x}{vector of data, \code{ts} object or \code{pdata.frame} object.}
    \item{reps}{number of replicates to generate.}
    \item{trim}{a list object containing the elements: \code{trim}, the trimming proportion;
\code{xmin}, the lower limit for left tail and \code{xmax}, the upper limit for right tail.}
    \item{reachbnd}{logical. If \code{TRUE} potentially reached bounds (xmin = smallest value - trimmed mean and xmax=largest value + trimmed mean) are given when the random draw happens to be equal to 0 and 1, respectively.}
    \item{expand.sd}{logical. If \code{TRUE} the standard deviation in the ensemble in expanded. See \code{\link{expand.sd}}.}
    \item{force.clt}{logical. If \code{TRUE} the ensemble is forced to satisfy the central limit theorem. See \code{\link{force.clt}}.}
    \item{scl.adjustment}{logical. If \code{TRUE} scale adjustment is performed
       to ensure that the population variance of the transformed series equals the variance of the data.}
    \item{sym}{logical. If \code{TRUE} an adjustment is peformed to ensure that the ME density is symmetric.}
    \item{elaps}{logical. If \code{TRUE} elapsed time during computations is displayed.}
    \item{colsubj}{the column in \code{x} that contains the individual index. It is ignored if the input data \code{x} is not a \code{pdata.frame} object.}
    \item{coldata}{the column in \code{x} that contains the data of the variable to create the ensemble. It is ignored if the input data \code{x} is not a \code{pdata.frame} object.}
    \item{coltimes}{an optional argument indicating the column that contains the times at which the observations for each individual are observed. It is ignored if the input data \code{x} is not a \code{pdata.frame} object.}
    \item{...}{possible argument \code{fiv} to be passed to \code{\link{expand.sd}}.}
  }

  \details{
    Seven-steps algorithm:
     \enumerate{
       \item Sort the original data in increasing order and store the ordering index vector.
       \item Compute intermediate points on the sorted series.
       \item Compute lower limit for left tail (\code{xmin}) and upper limit for right tail (\code{xmax}). This is done by computing the \code{trim} (e.g. 10%) trimmed mean of deviations among all consecutive observations. Thus the tails are uniformly distributed.
       \item Compute the mean of the maximum entropy density within each interval in such a way that the \emph{mean preserving constraint} is satisfied. (Denoted as \eqn{m_t} in the reference paper.) The first and last interval means have distinct formulas. See Theil and Laitinen (1980) for details.
       \item Generate random numbers from the [0,1] uniform interval and compute sample quantiles at those points.
       \item Apply to the sample quantiles the correct order to keep the dependence relationships of the observed data.
       \item Repeat the previous steps several times (e.g. 999).
     }

   The scale and symmetry adjustments are described in Vinod (2013) referenced below.

   In some applications, the ensembles must be ensured to be non-negative.
   Setting \code{trim$xmin = 0} ensures positive values of the ensembles. It also
   requires \code{force.clt = FALSE} and \code{expand.sd = FALSE}. These arguments are
   set to \code{FALSE} if \code{trim$xmin = 0} is defined and a warning is returned
   to inform that the value of those arguments were overwritten.
   Note: The choice of \code{xmin} and \code{xmax} cannot be arbitrary and should be
   cognizant of \code{range(x)} in data. Otherwise, if there are observations outside those
   bounds, the limits set by these arguments may not be met.
   If the user is concerned only with the trimming proportion, then it can be passed as argument
   simply \code{trim = 0.1} and the default values for \code{xmin} and \code{xmax} will be used.

  }

  \value{
    \item{x}{original data provided as input.}
    \item{ensemble}{maximum entropy bootstrap replicates.}
    \item{xx}{sorted order stats (xx[1] is minimum value).}
    \item{z}{class intervals limits.}
    \item{dv}{deviations of consecutive data values.}
    \item{dvtrim}{trimmed mean of dv.}
    \item{xmin}{data minimum for ensemble=xx[1]-dvtrim.}
    \item{xmax}{data x maximum for ensemble=xx[n]+dvtrim.}
    \item{desintxb}{desired interval means.}
    \item{ordxx}{ordered x values.}
    \item{kappa}{scale adjustment to the variance of ME density.}
    \item{elaps}{elapsed time.}
  }

  \references{
    Vinod, H.D. (2013), Maximum Entropy Bootstrap Algorithm Enhancements.
    \href{https://www.ssrn.com/abstract=2285041}{https://www.ssrn.com/abstract=2285041}.

    Vinod, H.D. (2006), Maximum Entropy Ensembles for Time Series Inference in Economics,
\emph{Journal of Asian Economics}, \bold{17}(6), pp. 955-978

    Vinod, H.D. (2004), Ranking mutual funds using unconventional utility theory and stochastic dominance, \emph{Journal of Empirical Finance}, \bold{11}(3), pp. 353-377.
  }

  \examples{
    ## Ensemble for the AirPassenger time series data
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, elaps=TRUE)

    ## Ensemble for T=5 toy time series used in Vinod (2004)
    set.seed(345)
    out <- meboot(x=c(4, 12, 36, 20, 8), reps=999, trim=0.25, elaps=TRUE)
    mean(out$ens)  # ensemble mean should be close to sample mean 16
  }

\keyword{ts}
