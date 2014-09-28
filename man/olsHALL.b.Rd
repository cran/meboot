\name{olsHALL.b}
 \alias{olsHALL.b}
  \title{OLS regression model for consumption}

  \description{
    Compute OLS coefficients in a regression model for the consumption variable. 
    See details.
  }

  \usage{
    olsHALL.b (y, x)
  }

  \arguments{
    \item{y}{dependent variable (consumption).}
    \item{x}{regressor variable (disposable income).}
  }

  \value{
      Coeffient estimates by OLS.
  }

  \details{
    The regression model is: c(t) = b1 + b2*c(t) + b3*y(t-1) + u(t), where
    'c' is consumption and 'y' is disposable income.

    This function is intended to speed up the ME bootstrap procedure
    for inference. Instead of using the \code{lm} or \code{dynlm} interfaces the 
    function calls directly to the Fortran procedure 'dqrls'.
  }

  \examples{
    data("USconsum")
    USconsum <- log(USconsum)

    # lm interface 
    lmcf1 <- lm(USconsum[-1,1] ~ USconsum[-51,1] + USconsum[-51,2])
    coefficients(lmcf1)
 
    # dynlm interface 
    library("dynlm")
    lmcf2 <- dynlm(consum ~ L(consum, 1) + L(dispinc, 1), data=USconsum)
    coefficients(lmcf2)

    # olsHALL.b
    olsHALL.b(y=USconsum[,1], x=USconsum[,2])
  }


