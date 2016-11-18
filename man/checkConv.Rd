\name{checkConv}
\alias{checkConv}
\title{Check Convergence}

\description{
This function generates a 3D array giving (Xn-X) in the notation of
the \code{ConvergenceConcepts} package by Lafaye de Micheaux and Liquet for sample paths
with dimensions \eqn{=} \code{n999} as first dimension, \code{nover} \eqn{=} range of n
values as second dimension and number of items in \code{key} as the third
dimension.  It is intended to be used for checking convergence of \code{meboot} in the context 
of a specific real world time series regression problem.
}

\usage{
checkConv (y, bigx, trueb = 1, n999 = 999, nover = 5, 
  seed1 = 294, key = 0, trace = FALSE) 
}

\arguments{
  \item{y}{vector of data containing the dependent variable.}
  \item{bigx}{vector of data for all regressor variables in a regression or \code{ts} object.
\code{bigx} should not include column of ones for the intercept.}
  \item{trueb}{true values of regressor coefficients for simulation. If \code{trueb=0} then use OLS 
coefficient values rounded to 2 digits as true values of beta for simulation purposes, to be close 
to but not exactly equal to OLS.}
  \item{n999}{number of replicates to generate in a simulation.}
  \item{nover}{number of values of n over which convergence calculated.}
  \item{seed1}{seed for the random number generator.}
  \item{key}{the subset of key regression coefficient whose convergence is studied
if \code{key=0} all coefficients are studied for convergence.}
  \item{trace}{logical. If \code{TRUE}, tracing information on the process is printed.}
}

\details{
Use this only when lagged dependent variable is absent.

Warning: \code{key=0} might use up too much memory for large regression problems.

The algorithm first creates data on the dependent variable for a simulation using known 
true values denoted by trueb.  It proceeds to create \code{n999} regression problems using the 
seven-step algorithm in \code{\link{meboot}} creating \code{n999} time series for all variable 
in the simulated regression.  It then creates sample paths over a range of n values for 
coefficients of interest denoted as \code{key} (usually a subset of original coefficients). 
For each key coefficient there are \code{n999} paths as n increases. If \code{meboot} algorithm 
is converging to true values, the value of (Xn-X) based criteria for 
"convergence in probability" and "almost sure convergence" in the notation of the 
\code{ConvergenceConcepts} package should decline. 
The decline can be plotted and/or tested to check if it is statistically significant 
as sample size increases. This function permits the user of \code{meboot} working with a short 
time series to see if the \code{meboot} algorithm is working in his or her particular situation.
}

\value{
A 3 dimensional array giving (Xn-X) for sample paths with dimensions \eqn{=} \code{n999} 
as first dimension, \code{nover} \eqn{=} range of n values as second dimension
and number of items in \code{key} as the third dimension ready for use in 
\code{ConvergenceConcepts} package.
}

\seealso{\code{\link{meboot}}, \code{\link[ConvergenceConcepts]{criterion}}.}

\references{
    Lafaye de Micheaux, P. and Liquet, B. (2009), Understanding Convergence Concepts: 
a Visual-Minded and Graphical Simulation-Based Approach,
\emph{The American Statistician}, \bold{63}(2) pp. 173-178.

    Vinod, H.D. (2006), Maximum Entropy Ensembles for Time Series Inference in Economics,
\emph{Journal of Asian Economics}, \bold{17}(6), pp. 955-978

    Vinod, H.D. (2004), Ranking mutual funds using unconventional utility theory 
and stochastic dominance, \emph{Journal of Empirical Finance}, \bold{11}(3), pp. 353-377.
}

\keyword{ts}

