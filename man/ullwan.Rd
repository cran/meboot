\name{ullwan}
 \docType{ullwan}
  \alias{ullwan}

  \title{Data about Some of the S\&P 500 Stock Prices}

  \description{
    This data set collects information about seven S\&P 500 stocks whose market capitalizaiton exceeds \$27 billion. The seven companies are labelled as ABT, AEG, ATI, ALD, ALL, AOL and AXP. For each company data from May 1993 to November 1997 are available (469 observations).
  }

   \format{The data are stored in an object of classes \code{pdata.frame} (a data.frame class with further attributes useful for panel data from the \code{plm} package) and \code{data.frame}.
   }

   \usage{
      data (ullwan)
   }

   \details{
     The following information is contained by columns:

     \itemize{
       \item \code{Subj}: Company index.  
       \item \code{Tim}: Times at which the data where observed (on a monthly basis).
       \item \code{MktVal}: Market capitalization. 
       \item \code{Price}: Stock prices.
       \item \code{Pupdn}: Binary variable, takes the value 1 if there is a turning point (a switch from a bull to a bear market or vice versa) and 0 otherwise.
       \item \code{Tb3}: Interest on 3-month Treasury bills.
     }
   }

   \source{Compustat database.}

   \references{
     Yves Croissant (2005). plm: Linear models for panel data. R package version 0.1-2. 
   }

\keyword{datagen}
