\name{flexMeboot}
\alias{flexMeboot}
\title{Flexible Extension of the Maximum Entropy Bootstrap Procedure}

\description{
This function extends the maximum entropy bootstrap procedure
implemented in \code{\link{meboot}}
to allow for for a flexible trend up, flat or down.
}

\usage{
  flexMeboot (x, reps = 9, segment = 5, forc = FALSE, myseq = seq(-1, 1, by = 1)) 
}

\arguments{
  \item{x}{vector of data, \code{ts} object.}
  \item{reps}{number of replicates to generate.}
  \item{segment}{block size.}
  \item{forc}{logical. If TRUE the ensemble is forced to satisfy the central limit theorem. 
See \code{\link{force.clt}}.}
  \item{myseq}{randomly chosen directions for trend.}
}

\details{
\code{flexMeboot} uses non-overlapping blocks having only m observations. 
A trend \eqn{a + bt} is replaced by \eqn{a + Bt}, 
where \code{B = sample(myseq) * b}.

Its steps are as follows:
\enumerate{
\item Choose block size \code{segment} denoted here as \eqn{m} 
(default equal to \eqn{m=5}) 
and divide the original time series \code{x} of length \eqn{T} 
into \eqn{k = floor(T/m)} blocks or subsets. Note that when
\eqn{T/m} is not an integer the \eqn{k}-th block will have a few more than 
\eqn{m} items. Hence let us denote the number of observations in each block as
\eqn{m} which equals \eqn{m} for most blocks, except the \eqn{k}-th.
\item Regress each block having m observations as subsets of \code{x} on the set
\eqn{\tau = 1, 2,..., m}, and store the intercept \eqn{b0}, 
the slope \eqn{b1} of \eqn{\tau} and the residuals \eqn{r}.
\item Note that the positive (negative) sign of the slope \eqn{b1} in this regression
determines the up (down) direction of the time series in that block.
Hence the next step of the algorithm replaces \eqn{b1} by \eqn{B1 = b1 * w}, defined
by a randomly chosen weight \eqn{w in (-1, 0, 1)}.
For example, when the random choice yields \eqn{w = -1}, the sign of \eqn{b1} is
reversed. Our weighting independently injects some limited flexibility
to the directions of values block segments of the original time series.
\item Reconstruct all time series blocks as: \eqn{b0 + b1 * w * \tau + r}, 
by adding back the residual \eqn{r} of the regression on \eqn{\tau}.
\item The next step applies the function \code{\link{meboot}} to each
block of time serie-now having a modified trend-and create a large
number, \eqn{J}, of resampled time series for each of the \eqn{k} blocks.
\item Sequentially join the \eqn{J} replicates of all \eqn{k} blocks or subsets together.
}
}

\value{
A matrix containing by columns 
the bootstrapped replicated of the original data \code{x}.
}

\seealso{\code{\link{meboot}}.}

\examples{
set.seed(235)
myseq <- seq(-1, 1, by = 0.5)
xx <- flexMeboot(x = AirPassengers, myseq = myseq, reps = 3)
matplot(cbind(AirPassengers, xx), type = "l")
}

\keyword{ts}
