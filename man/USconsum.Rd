\name{USconsum}
 \docType{data}
  \alias{USconsum}

  \title{Consumption and Disposable Income Data (Annual 1948-1998)}

  \description{
    Data set employed in Murray (2006, pp.46-47, 799-801) to discuss the Keynesian consumption function on the basis of the Friedman's permanent income hypothesis and Robert Hall's model.
   }

   \format{A .rda file storing the data as an \code{mts} object.}

   \usage{
      data (USconsum)
   }

   \details{
     Annual data. Available time series: (Each corresponding label in the list object appears in quotes.)

     \itemize{
       \item \code{consum}: consumption per capita in thousands of dollars (1948-1998). Log of this variable is the dependent variable and its lagged value is a regressor in Murray's Table 18.1. 
       \item \code{dispinc}: disposable income per capita in thousands of dollars (1948-1998).  Lagged value of the Log of this variable is the second regressor (proxy for permanent income) in Murray's Table 18.1.
     }
   }

   \source{Robert Hall's data for 1948 to 1977 extended by Murray to 1998 by using standard sources for US macroeconomic data from government publications (U.S. Bureau of Labor Statistics for population data, U.S. Bureau of Economic Analysis for income data, U.S. Bureau of Labor Statistics for consumption data).
   }

   \references{
      Murray, M.P. (2006), \emph{Econometrics. A modern introduction}, New York: Pearson Addison Wesley.
   }

\keyword{datagen}


