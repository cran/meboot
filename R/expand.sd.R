
expand.sd <- function(x, ensemble, fiv=5)
{
  sdx <- if (is.null(ncol(x)))
  sd(x) else apply(x, 2, sd)
  sdf <- c(sdx, apply(ensemble, 2, sd))
  sdfa <- sdf/sdf[1] # ratio of actual sd to that of original data
  sdfd <- sdf[1]/sdf # ratio of desired sd to actual sd
  
  # expansion is needed since some of these are <1 due to attenuation
  mx <- 1+(fiv/100)
  # following are expansion factors
  id <- which(sdfa < 1)
  if (length(id) > 0)
    sdfa[id] <- runif(n=length(id), min=1, max=mx)
  sdfdXsdfa <- sdfd[-1]*sdfa[-1]
  id <- which(floor(sdfdXsdfa) > 0)
  
  if (length(id) > 0)
  {
    if(length(id) > 1){
      ensemble[,id] <- ensemble[,id] %*% diag(sdfdXsdfa[id])
    } else {
      ensemble[,id] <- ensemble[,id] * sdfdXsdfa[id]
    }
  }
  
  if(is.ts(x)){
    ensemble <- ts(ensemble, frequency=frequency(x), start=start(x))
  }
  
  ensemble
}
