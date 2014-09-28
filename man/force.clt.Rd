\name{force.clt}
 \alias{force.clt}
  \title{Enforce Central Limit Theorem}

  \description{
    Function to enforce the maximum entropy bootstrap resamples to satisfy the central limit theorem.
  }

  \usage{
    force.clt (x, ensemble)
  }

  \arguments{
    \item{x}{a vector of data or a time series object.}
    \item{ensemble}{a matrix or \code{mts} object containing resamples of the original data \code{x}.}
  }

  \value{
    Revised matrix satisfying the central limit theorem.
  }

  \examples{
    set.seed(345)
    out <- meboot(x=AirPassengers, reps=100, trim=0.10, reachbnd=FALSE, elaps=TRUE)
    cm1 <- colMeans(out$ensemb)
    # Note that the column means are somewhat non-normal
    qqnorm(cm1)

    clt.ens <- force.clt(x=AirPassengers, ensemble=out$ensemble) 
    cm2 <- colMeans(clt.ens)
    # Note that the columns are closer to being normal
    qqnorm(cm2)

  }

\keyword{ts}


