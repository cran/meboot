\name{USfygt}
 \docType{data}
  \alias{USfygt}

  \title{Long-term Treasury Bond Rates and Deficit Data Set (Annual 1948-200)}

  \description{
    Data set employed in Murray (2006, pp.795-797) to test the null hypothesis that per capita federal deficits explain long-term Treasury bond interest rates based on the Stock and Watson's dynamic OLS model.
   }

   \format{A .rda file storing the data as an \code{mts} object.}

   \usage{
      data (USfygt)
   }

   \details{
     Annual data. Available time series: (Each corresponding label in the list object appears in quotes.)

     \itemize{
       \item "dy": mean changes in real per capita income (1949-1998).
       \item "fygt1": shorth-term (one-year) Treasury bond interest rates (1953-1998).
       \item "fygt10": long-term (ten-year) Treasury bond interest rates (1953-2000).
       \item "infl": inflation (1949-2000).
       \item "usdef": per capita real federal deficit (1948-2000).
       \item "reallir": real long term interest rates (not used in Murray's Table 18.12).
       \item "realsir": real short term interest rates (not used in Murray's Table 18.12).
     }
   }

   \source{Data was made available by James Stock and Mark Watson to readers of their famous Econometrica paper, 1993, 61, pp 783-820, who in turn used standard sources for US macroeconomic data from government publications.
   }

   \references{
      Murray, M.P. (2006), \emph{Econometrics. A modern introduction}, New York: Pearson Addison Wesley.
   }

\keyword{datagen}
